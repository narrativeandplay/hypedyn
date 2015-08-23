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
 var drag_startx, drag_starty, drag_endx, drag_endy;

var mouse = { x: 0, y: 0 }
var click_flip = false;
var dragged = false;
var click_thresh = 10; // how far apart can start and end of drag be to be considered a click
var drag_maxX = 0;
var drag_minX = 0;

var flip_canceled = false;

// useful cross browser block for bubbling after we have hit our desired target
function stopBubbling(e) {
	//disp("STOP BUBBLING");
	if (!e) var e = window.event;
	e.cancelBubble = true;
	if (e.stopPropagation) e.stopPropagation();
}

function flip_left(i) {
	//disp("flip left progress "+flips[i].progress);
	flips[i].dragging = true;
	flips[i].target = -1;
}
function flip_right(i) {
	//disp("flip right progress "+flips[i].progress);
	flips[i].dragging = true;
	flips[i].target = 1;
}
	
function finishFlipPage( flips ) {
	//disp("finish flip page ");
	function sq(x) { return x*x; };
	var isClick = sq( drag_minX - drag_maxX ) < sq( click_thresh );
	var page_width_half = page_width/2;
	// Figure out which page we should navigate to
	// clicking to turn pages forward (leftwards)
	if ( //drag_endx > page_width_half && 
		 drag_startx > page_width_half && 
		 isClick &&
		 //i == Math.floor(page) && 
		 page != flips.length - 1)
	{
		var i = Math.floor(page);
		flip_left(i);
	}
	// clicking to turn pages forward (rightwards)
	else if ( //drag_endx <= page_width_half && 
			  drag_startx <= page_width_half &&
			  isClick &&
			  //i == Math.floor(page) - 1 && 
			  page != 0)
	{
		var i = Math.floor(page) - 1;
		flip_right(i);
	}
	// drag release to flip forward
	else if ( //drag_endx <= page_width_half && 
			  drag_startx > page_width_half && 
			  !isClick &&
			  //i == Math.floor(page) && 
			  page != flips.length - 1 )
	{	
		var i = Math.floor(page);
		// drag successfully flip
		if ( drag_endx <= page_width_half ) {
			flip_left(i);
			//disp("dragging release forwards");
		// flip canceled
		} else {
			flip_right(i);
			flip_canceled = true;
			//disp("page drag leftward canceled");
		}
	}
	// drag release to flip backwards
	else if ( //drag_endx > page_width_half && 
			  drag_startx <= page_width_half && 
			  !isClick &&
			  //i == Math.floor(page) - 1 && 
			  page != 0)
	{
		var i = Math.floor(page) - 1;
		// drag successfully flip
		if ( drag_endx > page_width_half ) {
			flip_right(i);
			//disp("dragging release backwards");
		// flip canceled
		} else { 
			flip_left(i)
			flip_canceled = true;
			//disp("page drag rightwards canceled");
		}
	}
}

var mouseDown=0;
function mouseUpHandler(e) {
	e.preventDefault();
	stopBubbling(e);
	
	if ( mouseDown == 1) {
		mouseDown = 0;
		if (!clicked_link_flag && !flipping_flag ) {
			drag_endx = e.clientX;
			//disp('mouse up instant flip');
			finishFlipPage( flips );
		} else
			clicked_link_flag = false; // reset it (it has served its purpose
	}
}

function mouseDownHandler(e) {
	e.preventDefault();
	drag_startx = e.clientX;
	drag_minX = drag_startx;
	drag_maxX = drag_startx;
	mouseDown = 1;
}

var clickHandled = false; // flag to determine if a click was handled

// simulates mouse events on mobile 
// based on http://ross.posterous.com/2008/08/19/iphone-touch-events-in-javascript
function touchHandler(event)
{
	//disp("touch handling");
    var touches = event.changedTouches,
        first = touches[0],
        type = "";
		
	var canBubble = false;
	
	//disp("event type "+event.type);
    switch(event.type)
    {
        case "touchstart": type = "mousedown"; break;
        case "touchmove":  type = "mousemove"; canBubble = true; break;        
        case "touchend":   type = "mouseup"; break;
        default: return;
    }
	
    var simulatedEvent = document.createEvent("MouseEvent");
    simulatedEvent.initMouseEvent(type, canBubble, true, window, 1, 
                              first.screenX, first.screenY, 
                              first.clientX, first.clientY, false, 
                              false, false, false, 0/*left*/, null);

    clickHandled=false;
	first.target.dispatchEvent(simulatedEvent);
    // if the dispatched event resulted in clickedLink getting called, stop
    if(clickHandled) event.preventDefault();
    //disp("after dispatchEvent");
}

function draggingHandler(e) {
	//disp("stop bubb dragging");
	e.preventDefault();
	//stopBubbling(e);
	mouse.x = e.clientX;
	mouse.y = e.clientY;
	var page_width_half = page_width / 2;
	
	if (!clicked_link_flag && mouseDown > 0) {
		//disp("dragging");
		dragged = true;
		drag_minX = Math.min(drag_minX, e.clientX);
		drag_maxX = Math.max(drag_maxX, e.clientX);
		
		for( var i = 0; i < flips.length; i++ ) {
			// flipping forward
			if ( drag_startx > page_width_half && 
				 i == Math.floor(page) && 
				 page != flips.length - 1 )
			{
				flips[i].dragging = true;
				flips[i].target = (mouse.x - page_width_half) / page_width_half;
			}
			else if ( drag_startx <= page_width_half && 
					  i == Math.floor(page) - 1 && 
					  page != 0 )
			{
				flips[i].dragging = true;
				flips[i].target = (mouse.x - page_width_half) / (page_width_half);
			}
		}
	} else if (!clicked_link_flag)
		clicked_link_flag = false; // reset it (it has served its purpose
}

function mouseExitHandler(event) {
	event.preventDefault();
	// prevent mouse out getting triggered when mouse over children element 
	e = event.toElement || event.relatedTarget;
	
	if (e && e != null && mouseDown == 1) 
		//if (e.parentNode == this || e == this) { // this probably refers to the object mouseExitHandler is assigned to as onmouseout
		if (isParentOf( this, e) ) {
			return;
		} else { // dont trigger these when mouse out triggered by mouse over child element
			drag_endx = event.clientX;
			
			// reset mouseDown count
			//if ( event.clientX >= page_width/2 || event.clientX < page_width/2 )
			if (mouseDown == 1) {
				//disp("drag flip up");
				finishFlipPage(flips);
				mouseDown = 0;
			}
			mouseDown = 0;
		}
	function isParentOf ( parent, obj ) {
		//disp("parent of "+obj.tagName+" "+parent.tagName);
		//disp("parent name "+obj.getAttribute("name")+" "+parent.getAttribute("name"));
		if (e == this) 
			return true; // not sure what this means
	
		if ( obj.parentNode != null)
			if (obj.parentNode == parent)
				return true;
			else return isParentOf( parent, obj.parentNode );
		else 
			return false;
	}
}

function init_event_listeners () {
	var canvas = $("myCanvas");
	var pageflip_canvas = $("pageflip-canvas");
	
	$('popup').addEventListener('mousedown', popup_mouse_handler, false); // was popup
	$('lightsoff-canvas').addEventListener('mousedown', black_fade_out, false);	
	
	if (page_flipping_mode) {
		$("pages").addEventListener('mouseup', mouseUpHandler, false);
		$("pages").addEventListener('mousedown', mouseDownHandler, false);
		$("pages").addEventListener('mousemove', draggingHandler, false);
		$("pages").addEventListener('mouseout', mouseExitHandler, false);
		
		$("pages").addEventListener('touchstart', touchHandler, false);
		$("pages").addEventListener('touchend', touchHandler, false);
		$("pages").addEventListener('touchmove', touchHandler, false);
		
	} else {
		//disp("no mouse event added ");
	}
}

function popup_mouse_handler(e) { e.preventDefault(); }

function touchDebug(str) {
	var title="<br><text> Touch Debug </text><br>";
	document.getElementById("touch_debug").innerHTML = title+"<text>"+str+"</text>";
}

// test for touch events
	/*if ("ontouchstart" in document.documentElement) {
		document.body.ontouchend = touchHandler;//UPeventPasser;
		document.body.ontouchstart = touchHandler;//DOWNeventPasser;
	} else {
		document.body.onmouseup = UPeventPasser;
		document.body.onmousedown = DOWNeventPasser;
		$("pages").onmouseup = UPeventPasser;
		$("pages").onmousedown = DOWNeventPasser;
	}*/
	
// gotten from http://www.nogginbox.co.uk/blog/canvas-and-multi-touch
/*function getCoords(e) {
	if (e.offsetX) {
		// Works in Chrome / Safari (except on iPad/iPhone)
		return { x: e.offsetX, y: e.offsetY };
	} else if (e.layerX) {
		// Works in Firefox
		return { x: e.layerX, y: e.layerY };
	} else {
		// Works in Safari on iPad/iPhone
		return { x: e.pageX - cb_canvas.offsetLeft, y: e.pageY - cb_canvas.offsetTop };
		return { x: e.pageX - cb_canvas.offsetLeft, y: e.pageY - cb_canvas.offsetTop };
	}
}*/