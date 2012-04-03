// get document element easily
function $(id) { return document.getElementById(id); }
function disp(obj) { 
	console.log(obj);
}
//<center><canvas id="myCanvas" width="250" height="60"></canvas></center>
	//#pages section {	
	//	display: block;
	//	min-width: 320px;
	//	min-height: 480px;
	//	position: absolute;
	//	overflow: hidden;
	//}	
	
//<script type="application/javascript" src="iscroll-lite.js?v4"></script>

//#pages section>div {
//		display: block;
//		width: 320px;
//		height: 480px;
//	}

//#pages { max-width: 320px; }

//<div id="background">
//	<img src="paper.png" class="stretch" alt="" />
//</div>
//max-height: 480px; overflow: hidden}
//<meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"/>
var startNodeID;
var currNodeID;

var page_width = 320;
var page_height = 480;

//var page_break_height = 400;
var page_padding = 20;
var line_break_width = page_width  - 160; //- page_padding * 2

var button_panel_height = 50
var page_indicator_height = 50;
var btm_height = device_height - button_panel_height;// - page_indicator_height;

var text_area_height = btm_height - page_indicator_height;

var myScroll = null;

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
	// do the same for choice links
	//if (clinklist[linkID] != undefined) {
	//	clinklist[linkID].followed += 1;
	//	eventTrigger("clickedLink", clinklist[linkID]);
	//	clicked_link_flag = true;
	//}
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
	insertSorted( text_to_replace, [linkID, altcontent], comparator );
}
function findReplaceText(linkID) {
	var result;
	for (var i in text_to_replace) {
		if (text_to_replace[i][0] == linkID) {
			// need to differentiate between 
			// text from fact or just text
			if (typeof text_to_replace[i][1] == "string")
				result = text_to_replace[i][1];
			else if (typeof text_to_replace[i][1] == "number")
				result = factlist[text_to_replace[i][1]].value;
			else {
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
	//var node = nodelist.get(nodeID);
	var node = nodelist[nodeID];
	if (node != undefined) {
		
		// reset clicked_link_flag
		//disp("reset clicked link flag in gotoNode");
		clicked_link_flag = false;
		
		// event trigger
		eventTrigger("enteredNode", node);
		for (var i in node.links) {
			eventTrigger("enteredNode", node.links[i]); 
		}
		
		node.visited = true;
		currNodeID = nodeID;
		
		update_anywhere_visibility ();
		
		// from here on different from black_fade_in
		var last_page_flip;
		var last_page;
		
		// reset the flipping for the new node
		if ( flips.length > 0 ) {
			disp("flips.length "+flips.length);
			last_page_flip = ( flips.length > 0 ) ? flips[flips.length-1] : null;
			disp("last page flip "+last_page_flip);
			if ( last_page_flip != null ) {
				last_page = last_page_flip.page;
				disp("last page found ");
				// store this somewhere so it is not destroyed
				//$('last_page').appendChild(last_page);
				//$('pages').appendChild(last_page);
			}
		}
		
		flips = [];
		prev_choice_buttons = choice_buttons;
		disp("gotonode choice transfer ");
		for (i in prev_choice_buttons)
			disp("prev chioce "+prev_choice_buttons[i]);
		page = 0;
		
		//var htmlcode = htmlFormat( pageBreak( node.content ), node.links );
		var htmlcode = htmlFormat( node.content, node.links, false );
		//var anywherecode = anywherelink_htmlcode();
		//disp("htmlcode "+htmlcode);
		//disp("anywherecode "+ anywherecode);
		// $("pages").innerHTML = htmlcode; //+ "<br>" + anywherecode;
		$("pages").innerHTML = htmlcode; //+ "<br>" + anywherecode;
		
	//	if (last_page !=undefined ) {
		//	$('last_page').appendChild(last_page);
		//}
		
		// makes sure last_page not included in this list
		var page_list = document.getElementsByName('page');
		disp("page list len "+page_list.length);
		
		//for (i=0; i< page_list.length; i++) {
		//	$('pages').appendChild(page_list[i]);
		//}
		
		if (last_page != undefined) {
		
			//$('last_page').removeChild(last_page);
			//$('pages').appendChild(last_page);
			//flips[flips.length] = last_page_flip;
			
			// assuming page_list is returned in order of appearance
			// this insert before the first page
			
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
				//prev_choice_buttons[i].right_button.removeEventListener('mouseup');
			}
			
			// insert as first page
			$('pages').insertBefore(last_page, page_list[0]);
			page = 0;
			//$('pages').insertBefore(page_list[1], page_list[0]);
			
			//$('last_page').appendChild(last_page)
			//page_list.unshift(last_page);
			//disp("page_list len after unshift "+page_list.length);
			
			/*
			var page_cache = []
			//$('pages').appendChild(last_page);
			for (i=0; i<page_list.length; i++) {
				page_cache[i] = page_list[i];
				//$('pages').appendChild(page_list[i]);
			}
			//$('pages').appendChild(last_page);
			for (i=0; i<page_cache.length; i++) {
				$('last_page').appendChild(page_cache[i]);
			}
			
			//// SECOND ORDERING
			//$('pages').appendChild(last_page);
			var page_list2 = document.getElementsByName('page');
			disp("page list2 len "+page_list2.length);
			
			for (i=0; i<page_list2.length; i++) {
				page_cache[i] = page_list2[i];
				//$('pages').appendChild(page_list[i]);
			}
			$('pages').appendChild(last_page);
			//$('pages').appendChild(page_cache[0]);
			for (i=0; i<page_cache.length; i++) {
				$('pages').appendChild(page_cache[i]);
			}
			//$('pages').innerHTML = "";
			//$('pages').appendChild(last_page);
			//$('pages').appendChild(page_list[0]);
			//debug
			*/
			
			
			//if (page_list.length == 3) {
			//	$('pages').appendChild(page_list[0]);
			//}
			
			
			//for (i=0; i<page_list.length; i++) {
				//page_cache[i] = page_list2[i];
			//	$('pages').appendChild(page_list[i]);
			//}
			
			//flips.unshift(last_page_flip);
			//disp("last pagee html3 "+last_page.innerHTML);
			//disp('');
			//disp("current pages "+$('pages').innerHTML);
		}
		
		style_pages();
		order_pages(page);
		
		// set the page to flip forward
		if (last_page_flip != undefined) {
			flips[0].target = -1;
			flips[0].progress = 1;
			//page: pages[i],   // The page DOM element related to this flip
			flips[0].dragging = true;
			//disp("last pagee html "+last_page.innerHTML);
		}
		
		anywherelink_buttons();
		add_anywhere_button();
		//addChoiceLink(nodeID);
		replace_button_placeholder ();

		drawPageIndicator(page, flips.length, (last_page_flip != undefined));
		
		/*
		if (last_page_flip != undefined) 
			drawPageIndicator(page, flips.length, true);
		else
			drawPageIndicator(page, flips.length, false);
			*/
		
		//disp("code "+$("pages").innerHTML);
	}
}

function runhypedyn() {
	if (typeof startNodeID == "undefined") alert("Start Node not set!");
	else gotoNode(startNodeID);
}

function add_anywhere_button() {
	//disp("adding anywhere button "+activated_anywhere_buttons.length);
	$("buttons-panel").innerHTML = "";
	for (i in activated_anywhere_buttons) {	
		var nodeID = activated_anywhere_buttons[i];
		var nodeName = nodelist[nodeID].name;
		//disp("adding node with ID "+nodeID);
		//disp("node name "+nodeName);
		$("buttons-panel").innerHTML += "<button type='button' style='position: absolute; left: 0px; top: 5px; font: 16px/20px Helvetica, sans-serif;' onClick='popup(" + nodeID + ")'><font size=5>" + nodeName +  "</font></button>";
	}
	
	//option button
	// font: 16px/20px Helvetica, sans-serif;  text-shadow: 1px 1px 1px #000;
	//float: right; 
	//height: 40px;
	$("buttons-panel").innerHTML += "<button type='button' style='position: absolute; right: 0px; top: 5px; font: 16px/20px Helvetica, sans-serif;' onClick='option_callback()'><font size=5>Options</font></button>";
	
	//$("buttons-panel").innerHTML = "<button type='button' onClick='popup()'><font size=5>Test</font></button>  <button type='button' onClick='black_fade_out()'><font size=5>test2</font></button";
	//#button-panel {
	//		z-Index: -210;
	//		background-color: rgba(255,255,255,1);
	//		opacity: 1.0; 
	//	}
	
	$("buttons-panel").style.backgroundColor = "rgba(255,255,255,1)";
	$("buttons-panel").style.backgroundImage = "url(page_back_320_480.png)"
	$("buttons-panel").style.width = "320px";
	
	button_panel_height = $("buttons-panel").scrollHeight;
	//disp("butt panel height "+button_panel_height);
	
	
	
	btm_height = device_height - button_panel_height;
	text_area_height = btm_height - page_indicator_height;
	
	//disp("dev height "+device_height);
	//disp("button_panel_height "+button_panel_height);
	$('pages').style.height = text_area_height; //btm_height;
	$('pages').style.top = button_panel_height;
	
	$("pageflip-canvas").style.height = device_height;
	//disp("popup height "+device_height);
	
	//disp( "popup HEIGHT "+(device_height - button_panel_height * 2 ));
	$('popup').style.height = ( device_height - button_panel_height * 2 - 30 ) + "px"; 
	//$('popup').style.top = 15 + "px"; //(button_panel_height + 15)

	$('outer-popup').style.height = (device_height - button_panel_height * 2) + "px" ;
	//$('outer-popup').style.top = button_panel_height + "px";

	$("lightsoff-canvas").style.height = device_height;
	
	//$("pageflip-canvas").style.top = button_panel_height;
	//$("lightsoff-canvas").style.top = button_panel_height;
	
	
	activated_anywhere_buttons = [];
}

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
	//add_anywhere_button();
	init_event_listeners ();
	drawPageIndicator(page, page.length, false);
	
	//start_story();
	loadStory(); //defined in dynfile.js (the story data file)
	runhypedyn(); // entrance point of the story logic
	//setup_scroll();
	setTimeout('window.scrollTo(0, 0)', 1000); // for mobile to hide the url
	//setTimeout("window.scrollTo(0, 5)", 1000);
}