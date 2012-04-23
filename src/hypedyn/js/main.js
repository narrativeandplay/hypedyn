/* Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
 * 
 * Copyright (C) 2008-2011
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

/* misc functions */

// get document element easily
function $(id) { return document.getElementById(id); }
function disp(obj) { 
	console.log(obj);
}
function isNumber (o) {
  return ! isNaN (o-0);
}
function clone_arr ( arr ) {
	var result = [];
	for (i in arr) {
		result[i] = arr[i];
	}
	return result;
}

var device_width, device_height, page_width, btm_height, text_area_height;

// can be browser or mobile
var display_mode;
var page_flipping_mode;

// determine whether it is a browser or a mobile
function device_detection() {
	var element = document.createElement('div');
	if ("ontouchstart" in element) {
		display_mode = "mobile";
		page_flipping_mode = false; // mobile has page flipping removed as well
		disp("touch detected!");
	} else {
		display_mode = "browser";
		page_flipping_mode = false;
	}
}

/* global var */
var startNodeID;
var currNodeID;

var page_height = 480;
//var page_break_height = 400;
var page_padding = 20;

var button_panel_height = 50
var page_indicator_height = 50;

var myScroll = null;

function get_device_dimension() {
	disp("get device dimension");
	device_width = (window.innerWidth > 0) ? window.innerWidth : screen.width;
	device_height = (window.innerHeight > 0) ? window.innerHeight : screen.height;
	
	if (display_mode == "browser")
		page_width = device_width; // 320 hardcoded for mobile
	else if (display_mode == "mobile") {
		page_width = 320;
		if (page_flipping_mode)
			device_height = 410;
	}
	
	btm_height = device_height - button_panel_height;// - page_indicator_height;
	text_area_height = btm_height - page_indicator_height;
	disp("device height "+device_height);
	disp("btm_height "+btm_height);
	disp('text area height '+text_area_height);
	
	$('page-indicate-canvas').style.left = page_width/4;
	$('page-indicate-canvas').width = page_width/2;
	$('page-indicate-canvas').height = 50;
	if (page_flipping_mode)
		drawPageIndicator( page, flips.length, back_page_check() );
	
	$('lightsoff-canvas').width = page_width; //320
	$('lightsoff-canvas').style.width = page_width; //320
	$('lightsoff-canvas').height = 480;
	
	$('pageflip-canvas').width = page_width; //320
	$('pageflip-canvas').style.width = page_width; //320
	$('pageflip-canvas').height = 480;
	
}

function nonpageflip_init() {
	if (!page_flipping_mode) {
		$("pageflip-canvas").style.visibility = "hidden";
		$("lightsoff-canvas").style.visibility = "hidden";
		$("popup").style.visibility = "hidden";
		
		//document.body.style.backgroundImage = "url(page_back_320_480.png)";
		//document.body.style.backgroundSize = "100%"; 
		
		//$("outer-popup").style.backgroundColor = "rgba(100,100,255,1)";
		//$("popup").style.backgroundColor = "rgba(0,0,255,1)";
		//$('pages').style.visibility = "hidden";
	} 
}

// TOFIX, setFact getting triggered twice by hyper link
var clicked_link_flag = false;
function clickedLink(linkID) {
	disp("clicked link");
	if (linklist[linkID] != undefined) {
		linklist[linkID].followed += 1;
		eventTrigger("clickedLink", linklist[linkID]);
		//disp("clicked link true in clickedLink");
		clicked_link_flag = true;
	}
}

// replace text works as follows
// eventTrigger("nodeEntered", link) trigger all the replaceText that is suppose to fire
// replaceText place a pair of linkID and content into text_to_replace
// in htmlFormat use findReplaceText(linkID) to find out whether the link need replacing or not
// text_to_replace is emptied for next time
var text_to_replace = [];
function replaceText(linkID, altcontent) {
	function comparator ( pair1, pair2 ) {
		pair1[0] < pair2[0];
	}
	disp("replace text "+altcontent);
	insertSorted( text_to_replace, [linkID, altcontent], comparator );
}
function findReplaceText(linkID) {
	var result;
	for (var i in text_to_replace) {
		if (text_to_replace[i][0] == linkID) {
			// need to differentiate between 
			// text from fact or just text
			
			if (typeof text_to_replace[i][1] == "string") {
				disp("string");
				result = text_to_replace[i][1];
			} else if (typeof text_to_replace[i][1] == "number") {
				disp("number");
				result = factlist[text_to_replace[i][1]].value;
			} else {
				alert("typeof result "+ typeof text_to_replace[i][1]);
				result = "[Text Replace Error]";
			}
		}
	}
	return result;
}

var activated_anywhere_nodes = [];
var activated_anywhere_buttons = [];

// anywhere link stub TODO:
function addAnywhereLink(anywhereNodeID) {
	if (anywhereNodeID == currNodeID)
		return; // don't add this node when we're already there
	for ( i in activated_anywhere_nodes) {
		if (activated_anywhere_nodes[i].id == anywhereNodeID) {
			return; // break to prevent last line from running
		}
	}
	activated_anywhere_nodes[activated_anywhere_nodes.length] = nodelist[anywhereNodeID];
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

function gotoNode(nodeID) {
	disp("gotoNODE " +nodeID);
	var node = nodelist[nodeID];
	if (node != undefined) {
		clicked_link_flag = false;
		
		eventTrigger("enteredNode", node);
		for (var i in node.links) {
			eventTrigger("enteredNode", node.links[i]); 
		}
		
		node.visited = true;
		currNodeID = nodeID;
		
		update_anywhere_visibility ();
		
		if (page_flipping_mode) {
			// from here on different from black_fade_in
			var last_page_flip;
			var last_page;
			
			// reset the flipping for the new node
			if ( flips.length > 0 ) {
				last_page_flip = ( flips.length > 0 ) ? flips[flips.length-1] : null;
				if ( last_page_flip != null ) {
					last_page = last_page_flip.page;
				}
			}
			
			flips = [];
			prev_choice_buttons = choice_buttons;

			for (i in prev_choice_buttons)
				disp("prev chioce "+prev_choice_buttons[i]);
			page = 0;
		}
		
		var htmlcode = htmlFormat( node.content, clone_arr(node.links).concat(activated_anywhere_nodes), false );
		$("pages").innerHTML = htmlcode;
		
		if (page_flipping_mode) {
			// makes sure last_page not included in this list
			var page_list = document.getElementsByName('page');
			
			if (last_page != undefined) {
				
				// change font color to grey for all font in last_page
				last_page.style.color = "grey";
				for (i in prev_choice_buttons) {
					var left_button = prev_choice_buttons[i].left_button;
					var right_button = prev_choice_buttons[i].right_button;
					
					if (prev_choice_buttons[i].activated)
						left_button.fontcolor = "black";
					else 
						left_button.fontcolor = "grey";
						
					left_button.removeEventListener("mouseup", shrink, false);
					right_button.removeEventListener('mouseup', right_button.run_click_callback, false);
				}
				
				// insert as first page
				$('pages').insertBefore(last_page, page_list[0]);
				page = 0;
			}
			
			style_pages();
			order_pages(page);
			
			// set the page to flip forward
			if (last_page_flip != undefined) {
				flips[0].target = -1;
				flips[0].progress = 1;
				//page: pages[i],   // The page DOM element related to this flip
				flips[0].dragging = true;
			}
		} else {
			// do during browser mode
			style_pages();
		}
		
		//anywherelink_buttons(); // original anywhere nodes
		add_anywhere_button(); // choice links
		replace_button_placeholder();
		
		if (page_flipping_mode)
			drawPageIndicator(page, flips.length, (last_page_flip != undefined));
	}
}

function runhypedyn() {
	if (typeof startNodeID == "undefined") alert("Start Node not set!");
	else gotoNode(startNodeID);
}

function add_anywhere_button() {
	disp("add anywhere button");
	$("buttons-panel").innerHTML = "";
	for (i in activated_anywhere_buttons) {	
		var nodeID = activated_anywhere_buttons[i];
		var nodeName = nodelist[nodeID].name;
		$("buttons-panel").innerHTML += "<button type='button' style='position: absolute; left: 0px; top: 5px; font: 16px/20px Helvetica, sans-serif;' onClick='popup(" + nodeID + ")'><font size=5>" + nodeName +  "</font></button>";
	}
	
	//option button
	// font: 16px/20px Helvetica, sans-serif;  text-shadow: 1px 1px 1px #000; float: right; height: 40px;
	$("buttons-panel").innerHTML += "<button type='button' style='position: absolute; right: 0px; top: 5px; font: 16px/20px Helvetica, sans-serif;' onClick='option_callback()'><font size=5>Options</font></button>";
	
	//$("buttons-panel").style.backgroundColor = "rgba(255,0,0,1)";
	if (page_flipping_mode)
		$("buttons-panel").style.backgroundImage = "url(page_back_320_480.png)"
	
	$("buttons-panel").style.width = page_width;  // "320px" for mobile
	// new for browser version
	$("buttons-panel").style.backgroundRepeat = "repeat";//"no-repeat"; 
	
	button_panel_height = $("buttons-panel").scrollHeight;
	
	btm_height = device_height - button_panel_height;
	text_area_height = btm_height - page_indicator_height;
	
	$('pages').style.height = text_area_height;
	$('pages').style.top = button_panel_height;
	
	$("pageflip-canvas").style.height = device_height;
	$('popup').style.height = ( device_height - button_panel_height * 2 - 30 ) + "px";
	$('outer-popup').style.height = (device_height - button_panel_height * 2) + "px";
	$("lightsoff-canvas").style.height = device_height;
	
	activated_anywhere_buttons = [];
}

// options not implemented yet
var option_mode = false;
function option_callback() {
	if (!option_mode) {
		window.scrollTo(10, -50);
	} else {
		window.scrollTo(0, 0);
	}
	option_mode = !option_mode;
}

window.onload = function() {
	device_detection();
	get_device_dimension();
	init_event_listeners ();
	//drawPageIndicator(page, page.length, false);
	
	loadStory(); // defined in dynfile.js (the story data file)
	runhypedyn(); // entrance point of the story logic
	//setTimeout('window.scrollTo(0, 0)', 1000); // for mobile to hide the url
	
	nonpageflip_init();
}