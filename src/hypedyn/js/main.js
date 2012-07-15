/* Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
 * 
 * Copyright (C) 2008-2012
 * National University of Singapore
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

var device_width, device_height, page_width, btm_height, text_area_height;

// display mode can be 'browser' or 'mobile'
var display_mode;

// determine whether it is a browser or a mobile
function device_detection() {
	var element = document.createElement('div');
	if ("ontouchstart" in element) {
		display_mode = "mobile";
		page_flipping_mode = true; // mobile has page flipping removed as well
		//disp("touch detected!");
	} else {//browser
		display_mode = "browser";
		page_flipping_mode = false; //debug was false
	}
}

/* global var */
var startNodeID;
var currNodeID;
var prev_read_nodes = [];

var myScroll = null;

function get_device_dimension() {
	//disp("get device dimension");
	device_width = (window.innerWidth > 0) ? window.innerWidth : screen.width;
	device_height = (window.innerHeight > 0) ? window.innerHeight : screen.height;
	
	if (display_mode == "browser") {
		page_width = device_width; // 320 hardcoded for mobile
		//debug
		//page_width = 320;
		//device_height = 410;
	}else if (display_mode == "mobile") {
		page_width = 320;
		if (page_flipping_mode)
			device_height = 410;
	}
	
	btm_height = device_height - button_panel_height;// - page_indicator_height;
	text_area_height = btm_height - page_indicator_height;
	 //disp("device height "+device_height);
	 //disp("device width "+device_width);
	// disp("btm_height "+btm_height);
	// disp('text area height '+text_area_height);
	
	$('page-indicate-canvas').style.left = page_width/4;
	$('page-indicate-canvas').width = page_width/2;
	$('page-indicate-canvas').height = 50;
	if (page_flipping_mode)
		drawPageIndicator( page, flips.length, back_page_check() );
	
	$('lightsoff-canvas').width = page_width; //320
	$('lightsoff-canvas').style.width = page_width; //320
	//$('lightsoff-canvas').height = 480;
	
	$('pageflip-canvas').width = page_width; //320
	$('pageflip-canvas').style.width = page_width; //320
	//$('pageflip-canvas').height = 480;
	
	// make sure test_bed has same styling as actual page
	$('test_bed').className = "pagesdiv";
	$('test_bed').style.width = page_width;
}

function adjust_size() {

}

function nonpageflip_init() {
	if (!page_flipping_mode) {
		$("pageflip-canvas").style.visibility = "hidden";
		$("pageflip-canvas").style.zIndex = -1000;
		$("page-indicate-canvas").style.visibility = "hidden";
		$("page-indicate-canvas").style.zIndex = -1000;
		//$("lightsoff-canvas").style.visibility = "hidden";
		$("popup").style.visibility = "hidden";
	}
}

// TOFIX, setFact getting triggered twice by hyper link
var clicked_link_flag = false;
function clickedLink(linkID) {
	if ( linklist[linkID] ) {
		eventTrigger("clickedLink", linklist[linkID]);
		linklist[linkID].followed += 1; // NOTE: this must be AFTER eventTrigger
		//disp("clicked link true in clickedLink");
		clicked_link_flag = true;
        clickHandled = true; // tell touchHandler that the click was handled
	}
	var anywhere_node_check = get_activated_anywhere_node ( linkID );
	if ( anywhere_node_check ) {
		eventTrigger("enteredNode", anywhere_node_check); // linkID is actually anywhere node id
		gotoNode( linkID ); // anywhere node has no explicit goto actions in its rules.
		//disp("clicked link true in clickedLink");
		clicked_link_flag = true;
        clickHandled = true; // tell touchHandler that the click was handled
	}
}

// replace text works as follows
// eventTrigger("nodeEntered", link) trigger all the replaceText that is suppose to fire
// replaceText place a pair of linkID and content into text_to_replace
// in htmlFormat use findReplaceText(linkID) to find out whether the link need replacing or not

// text_to_replace is emptied for next time
var text_to_replace = [];
function replaceText(linkID, content_type, altcontent) {
	function comparator ( pair1, pair2 ) {
		pair1[0] < pair2[0];
	}
	insertSorted( text_to_replace, [linkID, content_type, altcontent], comparator );
}

function findReplaceText(linkID) {
	
	//disp("find replace text "+linkID);
	var result;
	for (var i in text_to_replace) {
		if (text_to_replace[i][0] == linkID) {
			// need to differentiate between 
			// text from fact or just text
			switch (text_to_replace[i][1]) {
				case "alternative text":
					result = text_to_replace[i][2]; 
					break;
				case "text fact":
					result = factlist[text_to_replace[i][2]].value; 
					break;
				case "number fact":
					disp("fact ID "+text_to_replace[i][2]);
					disp("fact value "+ factlist[text_to_replace[i][2]].value);
					disp("null test " + (factlist[text_to_replace[i][2]].value === null));
					disp("null test negate " + (! (factlist[text_to_replace[i][2]].value === null) ));
					if (! (factlist[text_to_replace[i][2]].value === null) ) {
						disp("setting value "+ factlist[text_to_replace[i][2]].value.toString() );
						result = factlist[text_to_replace[i][2]].value.toString(); 
					} else {
						disp("setting null ");
						result = "null";
					}
					break;
			}
		}
	}
	return result;
}

var activated_anywhere_nodes = [];
var activated_anywhere_buttons = [];
function get_activated_anywhere_node ( nodeID ) {
	function hasMatchingID ( node ) {
		return node.id == nodeID;
	}
	return arr_find ( activated_anywhere_nodes, hasMatchingID );
}


// anywhere link stub TODO:
function addAnywhereLink(anywhereNodeID) {
	if (anywhereNodeID == currNodeID)
		return; // don't add this node when we're already there
	for ( i in activated_anywhere_nodes) {
		if (activated_anywhere_nodes[i].id == anywhereNodeID) {
			return; // break to prevent last line from running
		}
	}
	activated_anywhere_nodes.push( nodelist[anywhereNodeID] );
}

function addAnywhereButton(anywhereNodeID) {
	for ( i in activated_anywhere_buttons) {
		if (activated_anywhere_buttons[i] == anywhereNodeID) {
			return; // break to prevent last line from running
		}
	}
	activated_anywhere_buttons[activated_anywhere_buttons.length] = anywhereNodeID;
}

// phasing out
function anywherelink_htmlcode () {
	var result = "";
	for ( i in activated_anywhere_nodes ) {
		if (activated_anywhere_nodes[i].id != currNodeID) {
			result += "<a href='javascript:void(0)' onMouseUp='gotoNode(" 
				+ activated_anywhere_nodes[i].id + ")'>"
				+ activated_anywhere_nodes[i].name + "</a><br>"
		}
	}
	return result;
}

// update anywhere visibility
function update_anywhere_visibility () {
	activated_anywhere_nodes = [];  // reset
	for ( i in nodelist ) {
		if (nodelist[i].anywhere == true)
			// triggering the rule with the addAnywhereLink action to fire if condition true
			eventTrigger("anywhereCheck", nodelist[i]); 
	}
}

function addChoiceLink(nodeID) {
	var node = nodelist[nodeID];
	if (node != undefined) {
		for (i in node.clinks) {
			addButton(node.clinks[i].name, "clickedLink("+ node.clinks[i].id+ ")");
		}
	}
}

function setStartNode(snodeID) {
	startNodeID = snodeID;
}

/*
	Note peek_back_mode and back button should not be used together
	peek_back_mode needs to be false for back button to work fine (with page flipping on mobile)
*/
var last_page_flip;
var last_page;
var peek_back_mode = false ;


function cache_peek_back() {
	// reset the flipping for the new node
	if ( flips.length > 0 ) {
		last_page_flip = ( flips.length > 0 ) ? flips[flips.length-1] : null;
		if ( last_page_flip != null ) {
			last_page = last_page_flip.page;
		}
	}
	prev_choice_buttons = choice_buttons;
}

function insert_peek_back() {
	// makes sure last_page not included in this list
	var page_list = document.getElementsByName('page');
	
	if (peek_back_mode && last_page != undefined) {
		
		// change font color to grey for all font in last_page
		last_page.style.color = "grey";
		
		// make choices from previous node unclickable
		for (i in prev_choice_buttons) {
			var left_button = prev_choice_buttons[i].left_button;
			var right_button = prev_choice_buttons[i].right_button;
			
			// give the grey out look
			if (prev_choice_buttons[i].activated)
				left_button.fontcolor = "black";
			else 
				left_button.fontcolor = "grey";
				
			left_button.removeEventListener("mouseup", shrink, false);
			right_button.removeEventListener('mouseup', right_button.run_click_callback, false);
		}
		
		// insert as first page
		$('pages').insertBefore( last_page, page_list[0] );
	}
	
	style_pages();
	order_pages( page );
	
	// set the page to flip forward
	if ( peek_back_mode && last_page_flip != undefined ) {
		flips[0].target = -1;
		flips[0].progress = 1;
		//page: pages[i],   // The page DOM element related to this flip
		flips[0].dragging = true;
	}
}

// check whether back_button is valid or not 
// it is invalid when we're at the first node
function back_button_check() {
	back_button = $("back_button");
	if (back_button) {
		if (prev_read_nodes.length < 1) // we're at the first node (wont be 0 normally..)
			back_button.setAttribute("disabled", "disabled");
		else
			back_button.removeAttribute("disabled"); //enable it 
	}
}

// note this function shares a huge chunk of similar code with gotoNode
function backPrevNode() {
	//disp("backprevnode");
	//disp("before pop " + prev_read_nodes.length);
	nodeID = prev_read_nodes.pop();
	//disp("after pop " + prev_read_nodes.length);
	//if (prev_read_nodes.length == 1) // we're at the first node
	//	$("back_button").setAttribute("disabled", "disabled");
	back_button_check();
	
	if (nodeID) { // if not undefined
		//disp("back's gotoNODE " +nodeID);
		var node = nodelist[nodeID];
		if (node) {
			clicked_link_flag = false;
			
			eventTrigger("enteredNode", node);
			for (var i in node.links) {
				eventTrigger("enteredNode", node.links[i]); 
			}
			//disp("after trigger");
			
			//node.visited = true;
			currNodeID = nodeID;
			
			update_anywhere_visibility();
			//disp("update vis");
			if (page_flipping_mode) {
				if (peek_back_mode)
					cache_peek_back();
				flips = [];
				page = 0;
			}
			//disp("page_flip things");
			
			//var htmlcode = htmlFormat( node.content, clone_arr(node.links).concat(activated_anywhere_nodes), false );
			htmlcode = node_to_html( node, activated_anywhere_nodes, false );
			$("pages").innerHTML = htmlcode;
			
			if (page_flipping_mode)
				insert_peek_back();
			else 
				style_pages();
			
			//anywherelink_buttons(); // original anywhere nodes
			add_anywhere_button(); // choice links
			replace_button_placeholder();
			
			if (page_flipping_mode)
				//drawPageIndicator(page, flips.length, (last_page_flip != undefined));
				drawPageIndicator(page, flips.length, back_page_check());
		}
	}
}

function restartStory() {
    currNodeID = undefined;
	prev_read_nodes = [];
	nodelist = [];
	linklist = [];
	rulelist = [];
	actionlist = [];
	conditionlist = [];
	loadStory();
	runhypedyn()
}

// refresh node
function refreshNode(node) {
    // trigger enteredNode for links to replace text
    for (var i in node.links) {
		eventTrigger("enteredNode", node.links[i]); 
    }

    // check anywhere nodes
	update_anywhere_visibility ();
		
	if (page_flipping_mode) {
		if (peek_back_mode)
			cache_peek_back();
        flips = [];
        page = 0;
    }
		
    // regenerate the node text
    var new_html_code = node_to_html( node, activated_anywhere_nodes, false );
    $("pages").innerHTML = new_html_code;

	if (page_flipping_mode)
		insert_peek_back();
	else 
		style_pages();
    
    //anywherelink_buttons(); // original anywhere nodes
    add_anywhere_button(); // choice links
    init_element_height();
    replace_button_placeholder();

    // draw the page indicator if necessary
    if (page_flipping_mode)
        drawPageIndicator(page, flips.length, back_page_check());
}

function gotoNode(nodeID) {
	//disp("gotoNODE " +nodeID);
	//disp("really here?");
	var node = nodelist[nodeID];
	//disp("node here "+node);
	if (node != undefined) {
		//disp("start of goto");
		clicked_link_flag = false;
		
        // update history list
        if (currNodeID != undefined)
            prev_read_nodes.push( currNodeID );

        // update back button state
		back_button_check();
        
        // update node state - should this go before or after triggering enteredNode events?
		node.visited = true;
		currNodeID = nodeID;

        // trigger enteredNode events on the node
		eventTrigger("enteredNode", node);

        // refresh the node display
        refreshNode(node);
	}
}

function runhypedyn() {
	if (typeof startNodeID == "undefined") alert("Start Node not set!");
	else gotoNode(startNodeID);
}

var back_button, restart_button;

function add_anywhere_button() {
	//disp("add anywhere button");
	//$("buttons-panel").innerHTML = ""; // clear any buttons from previous 
	
	// option button (not used anymore)
	// font: 16px/20px Helvetica, sans-serif;  text-shadow: 1px 1px 1px #000; float: right; height: 40px;
	/*var option_button = document.createElement("button");
	option_button.setAttribute("type", 'button');
	option_button.setAttribute("style", 'position: absolute; right: 0px; top: 5px; font: 16px/20px Helvetica, sans-serif;');
	option_button.setAttribute("onClick", 'option_callback()');
	option_button.innerHTML = "<font size=5>Options2</font>";
	$("buttons-panel").appendChild( option_button ); */
	
	// back button
	/*
	if ( ! back_button ) {
		//disp("cant find back button ");
		back_button = document.createElement("button");
		back_button.setAttribute("id", "back_button");
		back_button.setAttribute("type", 'button');
		back_button.setAttribute("style", 'position: static; left: 0px; top: 5px; font: 16px/20px Helvetica, sans-serif;');
		back_button.setAttribute("onClick", 'backPrevNode()');
		back_button.setAttribute("disabled", "disabled");
		back_button.innerHTML = "<font size=5>Back</font>";
	}
	$("buttons-panel").appendChild( back_button );
	
	// restart button
	if ( ! restart_button ) {
		//disp("cant find back button ");
		restart_button = document.createElement("button");
		restart_button.setAttribute("id", "restart_button");
		restart_button.setAttribute("type", 'button');
		restart_button.setAttribute("style", 'position: static; left: 0px; top: 5px; font: 16px/20px Helvetica, sans-serif;');
		restart_button.setAttribute("onClick", 'restartStory()');
		//restart_button.setAttribute("disabled", 'disabled');
		restart_button.innerHTML = "<font size=5>Restart</font>";
	}
	$("buttons-panel").appendChild( restart_button );
	*/
	
	// add anywhere button from the story
	for (i in activated_anywhere_buttons) {	
		var nodeID = activated_anywhere_buttons[i];
		var nodeName = nodelist[nodeID].name;
		
		var new_button = document.createElement("button");
		new_button.setAttribute("type", 'button');
		new_button.setAttribute("style", 'position: static; left: 0px; top: 5px; font: 16px/20px Helvetica, sans-serif;');
		new_button.setAttribute("onClick", 'popup(' + nodeID + ')'); // popup here
		//new_button.setAttribute("disabled", 'disabled');
		new_button.innerHTML = "<font size=5>" + nodeName +  "</font>";
		$("buttons-panel").appendChild( new_button );
		//$("buttons-panel").innerHTML += "<button type='button' style='position: absolute; left: 0px; top: 5px; font: 16px/20px Helvetica, sans-serif;' onClick='popup(" + nodeID + ")'><font size=5>" + nodeName +  "</font></button>";
	}
	activated_anywhere_buttons = [];
}

window.onload = function() {
	device_detection();
	get_device_dimension();
	
	init_event_listeners();
	
	loadStory(); // defined in dynfile.js (the story data file)
	read_config_flag();
	
	runhypedyn(); // entrance point of the story logic
	setTimeout('window.scrollTo(0, 0)', 1000); // for mobile to hide the url
	
	nonpageflip_init();
}