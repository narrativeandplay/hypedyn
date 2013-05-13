var page_height = 480;

var button_panel_height = 50
var page_indicator_height = 50;

// should be done after add_anywhere_button
// because we don't have the precise height of the button panel before we add the buttons in
// need to set manually because we dont have access to 
// [ page_width, device_height, button_panel_height ] inside css
function init_element_height() {
	//$("buttons-panel").style.backgroundColor = "rgba(255,0,0,1)";
	
	if (page_flipping_mode)
		$("buttons-panel").style.backgroundImage = "url(page_back_320_480.png)"
	
	$("buttons-panel").style.width = page_width - 10;  // 10 is for padding specified in styling.css
	$("buttons-panel").style.backgroundRepeat = "repeat";//"no-repeat"; 
	
	button_panel_height = $("buttons-panel").scrollHeight;
	btm_height = device_height - button_panel_height;
	text_area_height = btm_height - page_indicator_height;
	
	// page flip
	$('pages').style.height = text_area_height;
	//$('pages').style.top = button_panel_height;
	$("pageflip-canvas").style.height = device_height;
	
	// popup
	$('popup').style.height = ( device_height - button_panel_height * 2 - 30 ) + "px";
	$('outer-popup').style.height = ( device_height - button_panel_height * 2 ) + "px";
	
	$('popup').style.width = ( page_width - 90 ) + "px"; // 30 more than below (15 padding each side)
	$('outer-popup').style.width = ( page_width - 60 ) + "px";
	
	lo_canvas.width = page_width;
	lo_canvas.height = device_height;
	lo_canvas.style.width = page_width;
	lo_canvas.style.height = device_height;
	//lo_canvas = $("lightsoff-canvas");
	//lo_context = lo_canvas.getContext("2d");
}