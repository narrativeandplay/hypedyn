// *******************
// Page flipping code
// *******************

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
 
// based on http://www.html5rocks.com/en/tutorials/casestudies/20things_pageflip/ example code 
// used with permission

var flips = [];
var multiflips = [];
var page = 0;

// Render the page flip 60 times a second
setInterval( render, 1000 / 30 );

// has to be called after pages
// are in place in the page section
function order_pages(curr_page) {
	// List of all the page elements in the DOM
	
	var pages = document.getElementsByName("page");
	//var contents = document.getElementsByName("pagecontent");
	// Organize the depth of our pages and create the flip definitions
	for( var i = 0, len = pages.length; i < len; i++ ) {
		pages[i].style.zIndex = len - i; // pages at the back get smaller value
		if ( i < page )
			pages[i].style.width = 0;
		/*
		if (i == page) 
			pages[i].style.zIndex = 1;
		else if ( i == curr_page+1 ) 
			pages[i].style.zIndex = 0; // put target page behind
		//else if ( i == curr_page-1 ) 
		//	pages[i].style.zIndex = 0;	// put target page behind
		else if ( i < page ) {
			pages[i].style.zIndex = 2;
			pages[i].style.width = 0;
		}
		else 
			pages[i].style.zIndex = -1;
		*/
	}
}

function style_pages() {
	// List of all the page elements in the DOM
	var pages = document.getElementsByName("page");
	var contents = document.getElementsByName("pagecontent");
	// Organize the depth of our pages and create the flip definitions
	for( var i = 0, len = pages.length; i < len; i++ ) {
		
		//pages[i].style.backgroundColor = "rgba(0,255,0,1)";
		pages[i].className = 'pagesdiv'; 
		if (page_flipping_mode)
		//	pages[i].style.backgroundImage = "url(page_back_btm_320_480.png)";//"url(paper.png)";// no repeat;
			pages[i].className += ' pageflip';
		
		//pages[i].style.width = page_width;

		//dont limit the height when we're not breaking the page 
		if (page_flipping_mode)
			pages[i].style.height = btm_height;
			
		//pages[i].style.top = "50px";
		
		flips.push( {
			progress: 1,      // Current progress of the flip (left -1 to right +1)
			target: 1,        // The target value towards which progress is always moving
			page: pages[i],   // The page DOM element related to this flip
			dragging: false   // True while the page is being dragged
		} );
	}
	
	//disp("contents leng "+contents.length);
	for (var i = 0, len = contents.length; i < len; i++) {
		contents[i].className = 'pagecontent';
		//disp("setting page content width2 "+page_width);
		contents[i].style.width = (page_width - (2*20));
	}
}

// resize the div when we resize the windows
// this is a code snippet of the code in  style_pages() on top
function resize_page () {
	var contents = document.getElementsByName("pagecontent");
	for (var i = 0, len = contents.length; i < len; i++) {
		//contents[i].className = 'pagecontent';
		//disp("setting page content width2 "+page_width);
		contents[i].style.width = (page_width - (2*20));
	}
}

// cloning a flip into a multiflip with trailing/leading flips and put inside multiflips[]
function make_multiflip( flip, dir ) {
	
	var progress = [];
	switch (dir) {
		case "forw": // original starts with 1
			progress = [ flip.progress+0.5, flip.progress+1.25, flip.progress+1.8];
			break;
		case "back": // original starts with -1
			progress = [ flip.progress-0.5, flip.progress-1.25, flip.progress-1.8];
			break;
	}
	multiflips[0] = { progress: progress[0],
					  target: flip.target,
					  page: flip.page,
					  dragging: flip.dragging
					};
					
	multiflips[1] = { progress: progress[1],
					  target: flip.target,
					  page: flip.page,
					  dragging: flip.dragging
					};
	multiflips[2] = { progress: progress[2],
					  target: flip.target,
					  page: flip.page,
					  dragging: flip.dragging
					};
}

// if the flips inside are done reset multiflips array to empty arr
function multiflips_check() {	
	var result = 0;
	for (i in multiflips) {
		result += multiflips[i].progress;
	}
	if ( Math.abs(result) >= 3)
		multiflips = [];
}

var CANVAS_PADDING = 6;

var canvas = $("pageflip-canvas");
var context = canvas.getContext( "2d" );

var lo_canvas = $("lightsoff-canvas");
var lo_context = lo_canvas.getContext("2d");

var flipping_flag = false;
// TODO come up with a better way to check whether there is a previous node at the back
// currently i just check whether startNode and currNode is the same
// but one can possibly go back to the starting node
function render() {
	// Reset all pixels in the canvas
	context.clearRect( 0, 0, canvas.width, canvas.height );
	
	// concat flips with multiflips(cloned trailing flips)
	var temp_flips = flips;
	if (multiflips.length > 0)
		temp_flips = flips.concat(multiflips);
	
	for (i in temp_flips) {
		var flip = temp_flips[i] ;
		if ( flip.dragging )  {
			$("pageflip-canvas").style.zIndex = 100; // bring in front when we need to draw flip animation
			var step = 0
			// speed up easing towards the end 
			if ( Math.abs( flip.progress ) < 0.85 )
				step = ( flip.target - flip.progress ) *0.12;//* 0.2;
			else
				step = ( flip.target - flip.progress ) * 0.4;
				
			flip.progress += step; // was 0.2
			
			if ( Math.abs( flip.progress ) < 0.99 ) {
			
				if ( multiflips.length == 0 && peek_back_mode ) {
					if (step >=0 && //back (from page 1 to 0)
						i == 0 && 
						page == 1 &&
						currNodeID != startNodeID) //back
						make_multiflip( flip, "back" );
					else if (step < 0 && // forw (from page 0 to 1)
							i == 0 && 
							page == 0 &&
							currNodeID != startNodeID)
						make_multiflip( flip, "forw" );
				}
				if ( i < flips.length)
					drawFlip( flip, false );
				else 
					drawFlip( flip, true );
				 
				if (flip.progress >=0.5 && step >= 0 && i == page - 1) // backwards
					drawPageIndicator( page-1, flips.length, back_page_check() );
				else if ( flip.progress < 0.5 && step < 0 && i == page) //forward
					drawPageIndicator( page+1, flips.length, back_page_check() );
				//flipping_flag = true;
			} else if ( flip.progress >= 0.99 && step >= 0 ) {
				flip.progress = 1;
				flip.dragging = false;
				$("pageflip-canvas").style.zIndex = -100; // hide it behind again
				if (! flip_canceled && i < flips.length) { // i within flips, not a duplicate
					page--;
					order_pages(page);
					drawPageIndicator(page, flips.length, back_page_check() );
				} else if (flip_canceled) {
					flip_canceled = false;
					drawPageIndicator(page, flips.length, back_page_check() );
				}
			} else if ( flip.progress < -0.99 && step <= 0 ) {
				flip.progress = -1;

				flip.dragging = false;
				$("pageflip-canvas").style.zIndex = -100; // hide it behind again
				
				if (! flip_canceled && i < flips.length) {
					page++;
					order_pages(page);
					drawPageIndicator(page, flips.length, back_page_check() );
				} else if (flip_canceled) {
					flip_canceled = false;
					drawPageIndicator(page+1, flips.length, back_page_check() );
				}
			}
		}
	}
	
	multiflips_check();
	
	// black out 
	alpha_step();
	draw_black_out ();
}

function draw_black_out () {
	// also draw lightsout canvas
	// black out when needed
	lo_context.clearRect( 0, 0, lo_canvas.width, lo_canvas.height );
	lo_context.fillStyle = "rgba(0, 0, 0, "+ current_alpha + ")";
	lo_context.fillRect(0,0,lo_canvas.width,lo_canvas.height);
}

// multiflip_bool differentiates between cloned flips and original
// the only difference is we dont change the length of the page (hiding of the page) for multi flips
function drawFlip( flip , multiflip_bool) {

	var page_width_half = ( page_width * 0.5 );
	
	// Strength of the fold is strongest in the middle of the book
	var strength = 1 - Math.abs( flip.progress );
	
	// Width of the folded paper
	var foldWidth = page_width_half * ( 1 - flip.progress );
	
	// X position of the folded paper
	var foldX = page_width * flip.progress + foldWidth;
	//disp("flip progress "+flip.progress);
	
	var fold_diff = foldX - foldWidth;
	
	// How far the page should outdent vertically due to perspective
	var verticalOutdent = 20 * strength;
	
	// The maximum width of the left and right side shadows
	var paperShadowWidth = page_width_half * Math.max( Math.min( 1 - flip.progress, 0.5 ), 0 );
	var rightShadowWidth = page_width_half * Math.max( Math.min( strength, 0.5 ), 0 );
	var leftShadowWidth = page_width_half * Math.max( Math.min( strength, 0.5 ), 0 );
	
	// Change page element width to match the x position of the fold
	//flip.page.style.width = Math.max(foldX, 0) + "px";
	
	// only the original flip changes the width
	if (! multiflip_bool)
		//flip.page.style.width = Math.min(Math.max(foldX, 0), device_width)  + "px"; //- 40
		flip.page.style.width = Math.min(Math.max(foldX, 0), page_width)  + "px"; //- 40
	
	context.save();
	//context.translate( CANVAS_PADDING + ( BOOK_WIDTH / 2 ), PAGE_Y + CANVAS_PADDING );
	context.translate( 10, 6 );
	
	var half_verticalOutdent = verticalOutdent * 0.5;
	
	// Draw a sharp shadow on the left side of the page
	context.strokeStyle = 'rgba(0,0,0,'+(0.05 * strength)+')';
	context.lineWidth = 30 * strength;
	context.beginPath();
	context.moveTo(fold_diff, -half_verticalOutdent);
	context.lineTo(fold_diff, page_height + half_verticalOutdent);
	context.stroke();
	
	// Right side drop shadow
	/*
	var rightShadowGradient = context.createLinearGradient(foldX, 0, foldX + rightShadowWidth, 0);
	rightShadowGradient.addColorStop(0, 'rgba(0,0,0,'+(strength*0.2)+')');
	rightShadowGradient.addColorStop(0.8, 'rgba(0,0,0,0.0)');
	
	context.fillStyle = rightShadowGradient;
	context.beginPath();
	context.moveTo(foldX, 0);
	context.lineTo(foldX + rightShadowWidth, 0);
	context.lineTo(foldX + rightShadowWidth, page_height);
	context.lineTo(foldX, page_height);
	context.fill();
	
	// Left side drop shadow
	var leftShadowGradient = context.createLinearGradient(foldX - foldWidth - leftShadowWidth, 0, foldX - foldWidth, 0);
	leftShadowGradient.addColorStop(0, 'rgba(0,0,0,0.0)');
	leftShadowGradient.addColorStop(1, 'rgba(0,0,0,'+(strength*0.15)+')');
	
	context.fillStyle = leftShadowGradient;
	context.beginPath();
	context.moveTo(foldX - foldWidth - leftShadowWidth, 0);
	context.lineTo(foldX - foldWidth, 0);
	context.lineTo(foldX - foldWidth, page_height);
	context.lineTo(foldX - foldWidth - leftShadowWidth, page_height);
	context.fill();
	*/
	
	// Gradient applied to the folded paper (highlights & shadows)
	var foldGradient = context.createLinearGradient(foldX - paperShadowWidth, 0, foldX, 0);
	foldGradient.addColorStop(0.35, '#fafafa');
	foldGradient.addColorStop(0.73, '#eeeeee');
	foldGradient.addColorStop(0.9, '#fafafa');
	foldGradient.addColorStop(1.0, '#e2e2e2');
	
	context.fillStyle = foldGradient;
	context.strokeStyle = 'rgba(0,0,0,0.06)';
	context.lineWidth = 0.5;
	
	var double_verticalOutdent = (verticalOutdent * 2);
	// Draw the folded piece of paper
	context.beginPath();
	context.moveTo(foldX, 0);
	context.lineTo(foldX, page_height);
	context.quadraticCurveTo(foldX, page_height + double_verticalOutdent, fold_diff, page_height + verticalOutdent);
	context.lineTo(fold_diff, -verticalOutdent);
	context.quadraticCurveTo(foldX, -double_verticalOutdent, foldX, 0);
	
	context.fill();
	context.stroke();
	
	context.restore();
}

function drawCircle(context, cx, cy, r, color, mode) {

	mode = (mode == undefined) ? "fill" : mode;
	context.beginPath();
	context.arc(cx, cy, r, 0, 2 * Math.PI, false);
	context.closePath();
	
	switch (mode) {
		case "fill":
			context.fillStyle = color;
			context.fill();
			break;
		case "stroke":
			context.lineWidth = 2;
			context.strokeStyle = color;
			context.stroke();
			break;
	}
}
	
// tl - top left, br - btm right
function drawRect(context, tlx, tly, brx, bry, color, mode) {
	
	mode = (mode == undefined) ? "fill" : mode;
	context.beginPath();
	context.rect(tlx, tly, brx, bry);
	switch (mode) {
		case "fill":
			context.fillStyle = color;
			context.fill();
			break;
		case "stroke":
			context.lineWidth = 1;
			context.strokeStyle = color;
			context.stroke();
			break;
	}
}

function drawTriangle2( context, color ) {
	var canvas_width = context.canvas.width;
	var canvas_height = context.canvas.height;
	var width_third = canvas_width/3;
	var height_third = canvas_height/3;
	
	context.beginPath();
	context.moveTo(width_third, height_third);
	context.lineTo(width_third, height_third*2);
	context.lineTo(width_third*2, canvas_height/2);
	//context.lineTo(width_third, height_third);
	context.closePath();
	context.fillStyle = color;
	context.fill();
}


// not used at the moment 
// draw_back_page used to indicate whether we draw indicator of a backpage
function drawPageIndicator(curr_page, page_count, draw_back_page) {
	//disp("page indic "+curr_page);
	//disp("real page "+page);
	var canvas = document.getElementById("page-indicate-canvas");
	var gcontext = canvas.getContext("2d");
	
	gcontext.clearRect( 0, 0, canvas.width, canvas.height );
	
	// canvas.style.top = device_height - button_panel_height;
	// this is wrong but i dunno why this works the above suppose to be correct
	canvas.style.top = device_height - button_panel_height - page_indicator_height * 0.5;
	canvas.style.zIndex = 99;
	
	// boundary checking
	page_count = ( page_count == undefined || page_count == 0 ) ? 1 : page_count;
	curr_page = Math.min( curr_page, page_count-1 );
	
	var half_height = canvas.height / 2;
	var width = canvas.width;
	var radius = half_height * 0.15;
	
	// for debugging page indicator canvas position
	//drawRect(gcontext, 0, 0, canvas.width, canvas.height, "black", "stroke");
	
	for (var i=0; i<page_count; i++) {
		var color = (i == curr_page) ? "black" : "lightgrey"; //"blue" : "#8ED6FF";
		if (draw_back_page && i==0 && !(i == curr_page))
			drawCircle(gcontext, width/(page_count+1) * (i+1), half_height, radius, color, "stroke");
		else
			drawCircle(gcontext, width/(page_count+1) * (i+1), half_height, radius, color, "fill");
	}
	//drawRect(gcontext, 0, 0, canvas.width, canvas.height, "black");
}

/*
	black out animation
*/
// draw_black_out uses current_alpha this to draw the black background
var current_alpha = 0;
var target_alpha = 0;
var max_alpha = 0.5;
var transition_time = 500; // ms
var delta_time = transition_time / 30; 
var step_amt = 0;

// fading in or out the black background
function alpha_step() { 
	if ( Math.abs(step_amt) > 0 && current_alpha != target_alpha ) 
	{
		if ( ( step_amt > 0 && current_alpha < target_alpha )  ||
			 ( step_amt < 0 && current_alpha > target_alpha ))
		{
			current_alpha += step_amt; 
		}
		else if ( ( step_amt > 0 && current_alpha >= target_alpha ) ||
				  ( step_amt < 0 && current_alpha < target_alpha ) )
		{
			//terminating condition
			// it either stay black when alpha value is positive
			// or hidden to the back when alpha hits 0
			current_alpha = target_alpha;
			
			if (current_alpha == 0)
				$('lightsoff-canvas').style.zIndex = -100;
		}
	}
}

function popup(nodeID) { 
	//var node = nodelist.get(nodeID);
	var node = nodelist[nodeID];
	if (node != undefined) {
		//disp("popup "+node.links.length);
		// event trigger

        // trigger node rules for this node
		eventTrigger("enteredNode", node);
        
        // update node state
		node.visited = true;
		//currNodeID = nodeID; // this is causing the back button problem
        // only used in anywhere nodes, which aren't shown in a popup, so should be ok
		
        // trigger link rules (display only) for links in this node
		for (var i in node.links) {
			eventTrigger("enteredNode", node.links[i]); 
		}
        
        // check anywhere nodes
		update_anywhere_visibility();

		cleanup_scroll();
		var htmlcode = node_to_html( node, [], true );
		$('popup').innerHTML = "<div>" + htmlcode + "</div>";
		setup_scroll();
		
		// bring lighting canvas in front
		$('lightsoff-canvas').style.zIndex = 101;
		
		$('outer-popup').style.zIndex = 103;
		$('outer-popup').style.visibility = "visible";
		$('outer-popup').style.backgroundColor = "rgba(255,255,255,1.0)";
		
		$('popup').style.zIndex = 102;
		$('popup').style.visibility = "visible";
		$('popup').style.backgroundColor = "rgba(255,255,255,1.0)";
		
		target_alpha = max_alpha; 
		step_amt = (target_alpha - current_alpha) / delta_time;
		
	} else {
		alert("cant find popup node");
	}
}

// need to call before you change the content of your scrolling content
function cleanup_scroll() {
	if (myScroll != null) {
		myScroll.destroy();
		myScroll = null;
	}
}

function setup_scroll() {
	//disp("setup scroll");
	myScroll = new iScroll('popup',  { hScrollbar: false, vScrollbar: true, vScroll: true, bounce: false });
}

function black_fade_out(e) {
	//disp("fade out");
	target_alpha = 0;
	step_amt = (target_alpha - current_alpha) / delta_time;
	
	//$('popup').style.zIndex = -100;
	$('popup').style.visibility = "hidden";
	//$('outer-popup').style.zIndex = -101;
	$('outer-popup').style.visibility = "hidden";
}