var button_len = 250;
var button_height = "50px";
var button_side_len = 50;

function isNumber (o) {
  return ! isNaN (o-0);
}

var prev_choice_buttons = [];
var choice_buttons = [];

function back_page_check() {
	return !(prev_choice_buttons.length == 0);
}

// tl - top left, br - btm right
//function drawRect(context, tlx, tly, brx, bry) {
//	context.beginPath();
//	context.rect(tlx, tly, brx, bry);
//	context.lineWidth = 1;
//	context.strokeStyle = "black";
//	context.stroke();
//}

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
		
		//context.strokeStyle = "blue"; // stroke color
		//context.strokeText("Hello World!", 160, 25);
		
		context.fillStyle = this.fontcolor; //"blue";
		context.fillText( display_name, button_canvas.width/2, button_canvas.height/2 );
	}
	
	button_canvas.update = left_update;
	button_canvas.addEventListener( 'mouseup', shrink, false );
	
	//button_canvas.addEventListener('touchend', touchHandler, true);
	//button_canvas.addEventListener('touchmove', touchHandler, true);
	//button_canvas.addEventListener('touchstart', touchHandler, true);
	
	//button_canvas.onclick = shrink;
	
	button_canvas.target_width = button_len;
	button_canvas.start_width = button_len;
	
	button_canvas.fontcolor = "blue";
	button_canvas.update();
	
	function right_update() {
		var context = hidden_canvas.getContext('2d');
		drawRect(context, 0, 0, hidden_canvas.width, hidden_canvas.height, "black", "stroke");
		drawRect(context, 0, 0, hidden_canvas.width, hidden_canvas.height, this.bgcolor, "fill");
		drawTriangle2( context, "black" );
	}
	
	function run_click_callback(e) {
		//disp("stop bubb run click callback");
		//stopBubbling(e);
		this.bgcolor = "grey";
		this.update();
		//disp("clicked choice link");
		// did a slight delay of 10ms to show the grey highlight.
		setTimeout(dothis, 10);
		function dothis () {
			eval(click_callback);
			clicked_link_flag = false; // since we delayed, mouse up 
		}
	}
	
	hidden_canvas.update = right_update;
	hidden_canvas.addEventListener('mouseup', run_click_callback, false);
	hidden_canvas.run_click_callback = run_click_callback;
	//hidden_canvas.addEventListener('mouseup', expand, false);
	
	//hidden_canvas.addEventListener('touchend', touchHandler, true);
	//hidden_canvas.addEventListener('touchmove', touchHandler, true);
	//hidden_canvas.addEventListener('touchstart', touchHandler, true);
	//hidden_canvas.onclick = 
	
	hidden_canvas.target_width = 0;
	hidden_canvas.start_width = 0;

	hidden_canvas.update();
	
	hidden_canvas.bgcolor = "transparent";
	
	// keep a reference in of adjacent canvas in both of them
	button_canvas.hidden_canvas = hidden_canvas;
	hidden_canvas.button_canvas = button_canvas;
	
	//disp("last page AFT "+last_page.innerHTML);
	
	button_panel.appendChild(button_canvas);
	button_panel.appendChild(hidden_canvas);
	button_panel.appendChild(document.createElement('br'));
	
	//return button_panel;
	var button_obj = { left_button: button_canvas,
					   right_button: hidden_canvas,
					   button_div: button_panel,
					   //activate: button_canvas.update,
					   //deactivate: hidden_canvas.update,
					   activated: false
					};
	button_canvas.button_obj = button_obj;
	hidden_canvas.button_obj = button_obj;
	
	return button_obj;
}


// adds the button to the last page
function addButton( display_name, click_callback ) {
	
	var pages = document.getElementsByName("page");
	var last_page = pages[pages.length-1];//.getElementsByTag('div')[0];
	//disp("pages len " + pages.length);
	//disp("last page B4 " + last_page.innerHTML);
	
	var button = createButton( display_name, click_callback );
	last_page.appendChild(button.button_div);
}

function shrink(e) {
	//disp("shrink stop bubbb");
	stopBubbling(e);
	//disp("click link flag true in shrink");
	clicked_link_flag = true;
	//disp("SHRINK!");
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

var se_share_test = null;

function expand( button_canvas ) {
	//button_canvas = this.button_canvas;
	//console.log("expand!");
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
	for ( i in activated_anywhere_nodes ) {
		if (activated_anywhere_nodes[i].id != currNodeID) {
			//result += "<a href='javascript:void(0)' onMouseUp='gotoNode(" 
			//	+ activated_anywhere_nodes[i].id + ")'>"
			//	+ activated_anywhere_nodes[i].name + "</a><br>"
			addButton(activated_anywhere_nodes[i].name, "gotoNode("+ activated_anywhere_nodes[i].id+ ")");
		}
	}
}

var choice_link_placeholders = [];
var choice_link_cache = [];

// its a placeholder div element
function cache_choice_link_placeholder( ph ) {
	choice_link_placeholders[choice_link_placeholders.length] = ph;
}

// clink is the actual link with type 'choice'
// what is cached is the canvas choice button
function cache_choice_link( clink ) {
	var button = createButton( clink.name, "clickedLink(" + clink.id + ")");
	button.name = clink.name;
	choice_link_cache[choice_link_cache.length] = button;
}

// put choice buttons into the placeholder
function replace_button_placeholder () {
	var placeholders = document.getElementsByName("button-placeholders");
	for ( var i=0; i < placeholders.length; ++i ) {
		for ( j in choice_link_cache ) {
			if ( placeholders[i].id == choice_link_cache[j].name ) {
				//disp("placeholder name: "+placeholders[i].id);
				//disp("choice link name: "+choice_link_cache[j].name);
				//placeholders[i].style.overflow = "auto";
				placeholders[i].style.backgroundColor = "transparent";
				placeholders[i].innerHTML = "";
				placeholders[i].appendChild( choice_link_cache[j].button_div );
				choice_link_cache[j].button_div.style.zIndex = "auto";
				//disp("choice button zindex "+ choice_link_cache[j].button_div.style.zIndex);
			}
		}
	}
	
	// keep track of current nodes choices
	choice_buttons = choice_link_cache;
	
	//debug
	//disp("replace button placeholders");
	//for (i in choice_buttons) {
	//	disp("choice buttons "+choice_buttons[i]);
	//}
	
	choice_link_cache = [];
	//addButton(node.clinks[i].name, "clickedLink("+ node.clinks[i].id+ ")");
}

setInterval(adjustButtonWidth, 1000/button_frame_rate);