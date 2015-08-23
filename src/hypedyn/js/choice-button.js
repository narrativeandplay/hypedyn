/* Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
 * 
 * Copyright (C) 2008-2015
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
 
var button_len = 250;
var button_height = "50px";
var button_side_len = 50;

var prev_choice_buttons = [];
var choice_buttons = [];

function back_page_check() {
	return !(prev_choice_buttons.length == 0);
}

// click callback is javascript code in string form
function createButton( display_name, click_callback ) {
	// create the elements
	var button_panel = document.createElement('div');
	button_panel.setAttribute('name', 'button');
	
	var button_canvas = document.createElement('canvas');
	button_canvas.setAttribute('width', button_len);
	button_canvas.setAttribute('height', button_height);
	button_canvas.setAttribute('name', 'left_button');
	
	var hidden_canvas = document.createElement('canvas');
	hidden_canvas.setAttribute('width', 0);
	hidden_canvas.setAttribute('height', button_height);
	hidden_canvas.setAttribute('name', 'right_hidden_button');
	
	// callback and drawing
	function left_update() {
		var context = button_canvas.getContext('2d');
		
		drawRect(context, 0, 0, button_canvas.width, button_canvas.height, "black", "stroke");
		context.font = "15pt Calibri";
		context.lineWidth = 3;
		
		 // textAlign aligns text horizontally relative to placement
		context.textAlign = "center";
		
		// textBaseline aligns text vertically relative to font style
		context.textBaseline = "middle";	
		
		context.fillStyle = this.fontcolor; //"blue";
		context.fillText( display_name, button_canvas.width/2, button_canvas.height/2 );
	}
	
	function right_update() {
		var context = hidden_canvas.getContext('2d');
		drawRect(context, 0, 0, hidden_canvas.width, hidden_canvas.height, "black", "stroke");
		drawRect(context, 0, 0, hidden_canvas.width, hidden_canvas.height, this.bgcolor, "fill");
		drawTriangle2( context, "black" );
	}
	
	function run_click_callback(e) {
		//stopBubbling(e);
		this.bgcolor = "grey";
		this.update();
		// did a slight delay of 10ms to show the grey highlight.
		setTimeout(dothis, 10);
		function dothis () {
			eval(click_callback);
			clicked_link_flag = false; // since we delayed, mouse up 
		}
	}
	
	// set the draw update and mouse events listener
	button_canvas.update = left_update;
	button_canvas.addEventListener( 'mouseup', shrink, false );
	//button_canvas.addEventListener('touchend', touchHandler, true);
	//button_canvas.addEventListener('touchmove', touchHandler, true);
	//button_canvas.addEventListener('touchstart', touchHandler, true);
	
	button_canvas.target_width = button_len;
	button_canvas.start_width = button_len;
	
	button_canvas.fontcolor = "blue";
	button_canvas.update();
	
	// do the same for right canvas 
	hidden_canvas.update = right_update;
	hidden_canvas.addEventListener('mouseup', run_click_callback, false);
	hidden_canvas.run_click_callback = run_click_callback;
	//hidden_canvas.addEventListener('touchend', touchHandler, true);
	//hidden_canvas.addEventListener('touchmove', touchHandler, true);
	//hidden_canvas.addEventListener('touchstart', touchHandler, true);
	
	hidden_canvas.target_width = 0;
	hidden_canvas.start_width = 0;

	hidden_canvas.update();
	hidden_canvas.bgcolor = "transparent";
	
	// keep a reference in of adjacent canvas in both of them
	button_canvas.hidden_canvas = hidden_canvas;
	hidden_canvas.button_canvas = button_canvas;
	
	button_panel.appendChild(button_canvas);
	button_panel.appendChild(hidden_canvas);
	button_panel.appendChild(document.createElement('br'));
	
	//return button_panel;
	var button_obj = { left_button: button_canvas,
					   right_button: hidden_canvas,
					   button_div: button_panel,
					   activated: false
					};
	button_canvas.button_obj = button_obj;
	hidden_canvas.button_obj = button_obj;
	
	return button_obj;
}

// adds the button to the last page
function addButton( display_name, click_callback ) {
	
	var pages = document.getElementsByName("page");
	var last_page = pages[pages.length-1];
	
	var button = createButton( display_name, click_callback );
	last_page.appendChild(button.button_div);
}

function shrink(e) {
	stopBubbling(e);
	clicked_link_flag = true;
	this.start_width = this.width;
	this.target_width = button_len - button_side_len;
	this.button_obj.activated = true;
	
	// deactivate the rest
	for (i in choice_buttons)
		if ( choice_buttons[i].left_button != this) {
			expand(choice_buttons[i].left_button);
			choice_buttons[i].activated = false;
		}
}

function expand( button_canvas ) {
	button_canvas.start_width = button_canvas.width;
	button_canvas.target_width = button_len;
}

var button_shrink_time = 400;
var button_frame_rate = 30;

// animation of buttons
function adjustButtonWidth () {
	var buttons = document.getElementsByName('left_button');
	
	for ( i in buttons ) {
		var left_button = buttons[i];
		var right_hidden_button = left_button.hidden_canvas;
		if ( isNumber(i) ) {
			var step =  (left_button.target_width - left_button.start_width) / (button_shrink_time/button_frame_rate);
			if ( (left_button.width > left_button.target_width && step < 0) || 
				(left_button.width < left_button.target_width && step > 0))
			{
				left_button.width += step;
				left_button.update();
				right_hidden_button.width = button_len - left_button.width
				right_hidden_button.update();
			} else {
				left_button.width = left_button.target_width;
				right_hidden_button.width = button_len - left_button.target_width;
				left_button.update();
				right_hidden_button.update();
			}
		}
	}
}

function anywherelink_buttons () {
	var result = "";
	for ( i in activated_anywhere_nodes ) {
		if (activated_anywhere_nodes[i].id != currNodeID) {
			result += "<a href='javascript:void(0)' onMouseUp='gotoNode(" 
				+ activated_anywhere_nodes[i].id + ")'>"
				+ activated_anywhere_nodes[i].name + "</a><br>"
			//addButton(activated_anywhere_nodes[i].name, "gotoNode("+ activated_anywhere_nodes[i].id+ ")");
			
			//$("pages").innerHTML = 
			//var html_link = document.createElement('a');
			//html_link.setAttribute('href', 'javascript:void(0)');
			//html_link.setAttribute('onMouseUp', 'gotoNode(' + activated_anywhere_nodes[i].id + ')');
			//html_link.setAttribute('name', 'left_button');
			
			//var breaktag = document.createElement('br');
			
			//$('pages').innerHTML += "<a href='javascript:void(0)' onMouseUp='gotoNode(" 
			//	+ activated_anywhere_nodes[i].id + ")'>"
			//	+ activated_anywhere_nodes[i].name + "</a><br>"
			//disp("pages inner html "+ $('pages').innerHTML);
		}
	}
	return result;
}

var choice_link_cache = [];

// clink is the actual link with type 'choice'
// what is cached is the canvas choice button
function cache_choice_link( clink ) {
	var button = createButton( clink.name, "clickedLink(" + clink.id + ")");
	button.name = clink.name;
	choice_link_cache[choice_link_cache.length] = button;
}

// put choice buttons into the placeholder
// NOTE: the choice links had been placed from the htmlFormat linkcode section
function replace_button_placeholder () {
	var placeholders = document.getElementsByName("button-placeholders");
	for ( var i=0; i < placeholders.length; ++i ) {
		for ( j in choice_link_cache ) {
			if ( placeholders[i].id == choice_link_cache[j].name ) {
				placeholders[i].style.backgroundColor = "transparent";
				placeholders[i].innerHTML = "";
				placeholders[i].appendChild( choice_link_cache[j].button_div );
				choice_link_cache[j].button_div.style.zIndex = "auto";
			}
		}
	}
	// keep track of current nodes choices (for activating and deactivating only one at a time)
	choice_buttons = choice_link_cache;
	choice_link_cache = [];
}

setInterval(adjustButtonWidth, 1000/button_frame_rate);