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
 
var page_flipping_mode; // page break and flip animation
var back_button_flag = true;
var restart_button_flag = true;

var window_resize_flag = true;
var fixed_page_width = 0;
var fixed_page_height = 0;

function write_config_flag( flag_name, value ) {
	//disp("write config flag "+value);
	switch ( flag_name ) {
		case "window_resize_flag":
			if (display_mode == "browser") // only override if its a browser
				window_resize_flag = value; break;
		case "back_button_flag":
			back_button_flag = value; break;
		case "restart_button_flag":
			restart_button_flag = value; break;
		case "page_flipping_mode":
			if (display_mode == "mobile") // only override if its a mobile
				page_flipping_mode = value; break;
	}
}

function read_config_flag() {
	// if disabled remove it
	if ( $("buttons-panel") ) {
		if ( ! back_button_flag && $( "back_button" ) )
			$("buttons-panel").removeChild( $( "back_button" ) );
		if ( ! restart_button_flag && $( "restart_button" ) )
			$("buttons-panel").removeChild( $( "restart_button" ) );
	}
	
	if ( window_resize_flag )
		window.onresize = function () { 
			get_device_dimension();
			init_element_height();
			resize_page ();
		}
	else { // set the dimension once and size to that
		page_width = fixed_page_width;
		device_height = fixed_page_height;
		init_element_height();
		resize_page ();
	}
}