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

// works the same as scheme's find
// pred is a function that takes an object and returns a boolean
function arr_find ( arr, pred ) {
	for ( i in arr ) {
		if ( pred( arr[i] ) )
			return arr[i];
	}
	return undefined;
}

// this is time/cpu consuming but works to block the thread
function sleep(delay) {
    //var start = new Date().getTime();
	var unlocked = false;
	var accuracy_ms = 1000; // accurate to 10 ms
	
	function check_delay () {
		//if ( new Date().getTime() < start + delay )
		delay -= accuracy_ms;
		//disp("tick "+delay);
		if ( delay < 0 )
			return;
	}
	setInterval( check_delay, accuracy_ms ); 
}