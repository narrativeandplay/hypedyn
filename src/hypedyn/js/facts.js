/* Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
 * 
 * Copyright (C) 2008-2012
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
 
 var factlist = [];

function createFact(name, type, id) {
	newfact = new Object();
	newfact.name = name;
	newfact.type = type;
	newfact.value = null;
	
	if (id == undefined) {
		id = genID(factlist);
	}
	
	// default value for number
	if ( newfact.type === "number" )
		newfact.value = 0;
	
	newfact.id = id;
	factlist[id] = newfact;
	
	return newfact;
}

// assume value is of the correct type
function setFact( id, value) {
	disp("set fact "+id);
	disp(" value "+value);
	var fact = factlist[id];
	fact.value = value;
    
    // refresh the node - this is very inefficient, move it out
    node=nodelist[currNodeID];
    if(node != undefined)
        refreshNode(node);
	//factsDebug();
}

function getFact( id ) {
	return factlist[id];
}

// args is an array
function setNumberFact( id, mode, args ) {
	disp("set num fact "+id + " " + mode + " " + args);
	switch ( mode ) {
		case "Input": 
			setFact( id, args[0] );
			break;
		case "Fact":
			setFact( id, getFact( args[0] ).value );
			break;
		case "Math":
			var operator = args[0];
			var operand1 = args[1];
			var operand1_type = args[2];
			var operand2 = args[3];
			var operand2_type = args[4];
			
			// the real value
			var operand1_val, operand2_val;
			switch ( operand1_type ) {
				case "Input":
					operand1_val = operand1;
					break;
				case "Fact":
					operand1_val = getFact( operand1 ).value;
					break;
			}
			switch ( operand2_type ) {
				case "Input":
					operand2_val = operand2;
					break;
				case "Fact":
					operand2_val = getFact( operand2 ).value;
					break;
			}
			
			setFact( id, eval( operand1_val.toString() + operator + operand2_val.toString() ) );
	}
}



// return the html code 
function factsDebug() {
	var temp="";
	for (i in factlist) {
		temp += factlist[i].id+" "+factlist[i].name+"::  "+factlist[i].value+"<br>";
	}
	var title="<br><text> Fact Debug </text><br>";
	document.getElementById("fact_debug").innerHTML = title+"<text>"+temp+"</text>"+"<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><text>DEBUG</text>";
}

