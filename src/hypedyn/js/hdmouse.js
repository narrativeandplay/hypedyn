var drag_startx, drag_starty, drag_endx, drag_endy;

var mouse = { x: 0, y: 0 }
var click_flip = false;
var dragged = false;
var click_thresh = 10; // how far apart can start and end of drag be to be considered a click
var drag_maxX = 0;
var drag_minX = 0;

var flip_canceled = false;

// gotten from http://www.nogginbox.co.uk/blog/canvas-and-multi-touch
function getCoords(e) {
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
}

// useful cross browser block for bubbling after we have hit our desired target
function stopBubbling(e)
{
	//disp("STOP BUBBLING");
	if (!e) var e = window.event;
	e.cancelBubble = true;
	if (e.stopPropagation) e.stopPropagation();
}

function flip_left(i) {
	disp("flip left progress "+flips[i].progress);
	flips[i].dragging = true;
	flips[i].target = -1;
}
function flip_right(i) {
	disp("flip right progress "+flips[i].progress);
	flips[i].dragging = true;
	flips[i].target = 1;
}
	
function finishFlipPage( flips ) {
	//disp("finish flip page ");
	function sq(x) { return x*x; };
	var isClick = sq( drag_minX - drag_maxX ) < sq( click_thresh );
		
	// Figure out which page we should navigate to
	// clicking to turn pages forward (leftwards)
	if ( //drag_endx > PAGE_WIDTH / 2 && 
		 drag_startx > PAGE_WIDTH / 2 && 
		 isClick &&
		 //i == Math.floor(page) && 
		 page != flips.length - 1)
	{
		var i = Math.floor(page);
		flip_left(i);
	}
	// clicking to turn pages forward (rightwards)
	else if ( //drag_endx <= PAGE_WIDTH / 2 && 
			  drag_startx <= PAGE_WIDTH / 2 &&
			  isClick &&
			  //i == Math.floor(page) - 1 && 
			  page != 0)
	{
		var i = Math.floor(page) - 1;
		flip_right(i);
	}
	// drag release to flip forward
	else if ( //drag_endx <= PAGE_WIDTH / 2 && 
			  drag_startx > PAGE_WIDTH / 2 && 
			  !isClick &&
			  //i == Math.floor(page) && 
			  page != flips.length - 1 )
	{	
		var i = Math.floor(page);
		// drag successfully flip
		if ( drag_endx <= PAGE_WIDTH / 2 ) {
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
	else if ( //drag_endx > PAGE_WIDTH / 2 && 
			  drag_startx <= PAGE_WIDTH / 2 && 
			  !isClick &&
			  //i == Math.floor(page) - 1 && 
			  page != 0)
	{
		var i = Math.floor(page) - 1;
		// drag successfully flip
		if ( drag_endx > PAGE_WIDTH / 2 ) {
			flip_right(i);
			disp("dragging release backwards");
		// flip canceled
		} else { 
			flip_left(i)
			flip_canceled = true;
			disp("page drag rightwards canceled");
		}
	}
/*	else {
		disp(" drag_start "+ drag_startx);
		disp(" drag_end "+ drag_endx);
		disp(" isClick "+isClick);
		disp(" flip len "+ flips.length);
		disp(" page "+page);
	}*/
	
	//}
}

var mouseDown=0;
function mouseUpHandler(e) {
	// UP
	disp("pages UP");
	e.preventDefault();
	//disp("stop bub mouse UP");
	stopBubbling(e);
	
	//disp("clicked link flag "+ clicked_link_flag);
	//disp("flipping_flag "+flipping_flag);
	
	if ( mouseDown == 1) {
		mouseDown = 0;
		if (!clicked_link_flag && !flipping_flag ) {
			drag_endx = e.clientX;
			disp('mouse up instant flip');
			finishFlipPage( flips );
			
		} else {
			//disp("reset clicked_link_flag in mouse up");
			//disp("clicked link flag2 "+ clicked_link_flag);
			//disp("flipping_flag2 "+flipping_flag);
			clicked_link_flag = false; // reset it (it has served its purpose
		}
	}
}

function mouseDownHandler(e) {
	e.preventDefault();
	//disp("mouseDown stop bubb");
	//stopBubbling(e);
	//disp("DOWN");;
	drag_startx = e.clientX;
	drag_minX = drag_startx;
	drag_maxX = drag_startx;
	mouseDown = 1;
}

var dispatch_flag = false;


//debug
var start_cache = null;
var end_cache = null;

//http://ross.posterous.com/2008/08/19/iphone-touch-events-in-javascript
function touchHandler(event)
{
    var touches = event.changedTouches,
        first = touches[0],
        type = "";
		
	var canBubble = false;
	
	//disp("event type "+event.type);
    switch(event.type)
    {
        case "touchstart": type = "mousedown"; break;
        case "touchmove":  type = "mousemove"; canBubble = true; break;        //event.preventDefault()
        case "touchend":   type = "mouseup"; break;
        default: return;
    }
	
    //initMouseEvent(type, canBubble, cancelable, view, clickCount, 
    //           screenX, screenY, clientX, clientY, ctrlKey, 
    //           altKey, shiftKey, metaKey, button, relatedTarget);
    
    var simulatedEvent = document.createEvent("MouseEvent");
    simulatedEvent.initMouseEvent(type, canBubble, true, window, 1, 
                              first.screenX, first.screenY, 
                              first.clientX, first.clientY, false, 
                              false, false, false, 0/*left*/, null);
							  
	var theTarget = first.currentTarget;
	theTarget = (theTarget == undefined) ? this : null;
	disp("theTarget "+theTarget);
	disp("target zindex "+ theTarget.style.zIndex);
	//simulatedEvent.preventDefault();
	//$("pages").dispatchEvent(simulatedEvent);
	// was target 
	//theTarget.dispatchEvent(simulatedEvent);
	
	first.target.dispatchEvent(simulatedEvent);
}

function draggingHandler(e) {
	//disp("stop bubb dragging");
	e.preventDefault();
	//stopBubbling(e);
	mouse.x = e.clientX;
	mouse.y = e.clientY;
	
	if (!clicked_link_flag && mouseDown > 0) {
		//disp("dragging");
		dragged = true;
		drag_minX = Math.min(drag_minX, e.clientX);
		drag_maxX = Math.max(drag_maxX, e.clientX);
		
		for( var i = 0; i < flips.length; i++ ) {
			// flipping forward
			if ( drag_startx > PAGE_WIDTH / 2 && 
				 i == Math.floor(page) && 
				 page != flips.length - 1 )
			{
				flips[i].dragging = true;
				flips[i].target = (mouse.x - PAGE_WIDTH/2) / (PAGE_WIDTH/2);
				//disp("target set to "+flips[i].target);
				
				// fail safe to set flag back to false
				//setTimeout("flipping_flag = false", 1200);
			}
			else if ( drag_startx <= PAGE_WIDTH / 2 && 
					  i == Math.floor(page) - 1 && 
					  page != 0 )
			{
				flips[i].dragging = true;
				flips[i].target = (mouse.x - PAGE_WIDTH/2) / (PAGE_WIDTH/2);
				//disp("target set to "+flips[i].target);
				
				// fail safe to set flag back to false
				//setTimeout("flipping_flag = false", 1200);
			}
		}
	} else if (!clicked_link_flag) {
		//disp ("reset clicked flag in dragging");
		clicked_link_flag = false; // reset it (it has served its purpose
	}
}

function mouse_exit(event) {
	//disp("mouse exit stop bubb");
	event.preventDefault();
	//stopBubbling(event);
	// prevent mouse out getting triggered when mouse over children element 
	e = event.toElement || event.relatedTarget;
	
	if (e && e != null && mouseDown == 1) 
		//if (e.parentNode == this || e == this) { // this probably refers to the object mouse_exit is assigned to as onmouseout
		if (isParentOf( this, e) ) {
			return;
		} else { // dont trigger these when mouse out triggered by mouse over child element
			
			drag_endx = event.clientX;
			
			// reset mouseDown count
			//if ( event.clientX >= page_width/2 || event.clientX < page_width/2 )
			if (mouseDown == 1) {
				disp("drag flip up");
				finishFlipPage(flips);
				mouseDown = 0;
			} else {
				//disp("mouse exit "+event.clientX);
				//disp(" mouse exit ignored ");
				//disp("mouseDown "+mouseDown);
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
	
	/*if ("ontouchstart" in document.documentElement)
	{
		document.body.ontouchend = touchHandler;//UPeventPasser;
		document.body.ontouchstart = touchHandler;//DOWNeventPasser;
	}
	else
	{
		document.body.onmouseup = UPeventPasser;
		document.body.onmousedown = DOWNeventPasser;
		$("pages").onmouseup = UPeventPasser;
		$("pages").onmousedown = DOWNeventPasser;
	}*/
	
	//addEventListener('click',doSomething2,true)
	$("pages").addEventListener('mouseup', mouseUpHandler, false);
	$("pages").addEventListener('mousedown', mouseDownHandler, false);
	$("pages").addEventListener('mousemove', draggingHandler, false);
	$("pages").addEventListener('mouseout', mouse_exit, false);
	
	$("pages").addEventListener('touchstart', touchHandler, false);
	$("pages").addEventListener('touchend', touchHandler, false);
	$("pages").addEventListener('touchmove', touchHandler, false);
	
	$('popup').addEventListener('mousedown', popup_mouse_handler, false); // was popup
	$('lightsoff-canvas').addEventListener('mousedown', black_fade_out, false);
}

function popup_mouse_handler(e) {
	e.preventDefault();
}

function touchDebug(str) {
	var title="<br><text> Touch Debug </text><br>";
	document.getElementById("touch_debug").innerHTML = title+"<text>"+str+"</text>";
}