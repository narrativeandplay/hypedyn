<!-- hide script from old browsers
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

//importScript("hashtable.js");

// [] is shorthand for new Array();
var nodelist =[];
var linklist = [];
//var anywhere_nodelist = [];
var start_node;

//var clinklist = [];
	
  // datastructure
function createNode (name, content, anywhere, id ) {
	newNode = new Object();
	newNode.name = name;
	newNode.content = content;
	newNode.visited = false;
	newNode.links = [];
	newNode.rules = [];
	newNode.clinks = [];
	newNode.label = name;  // allow setting of label when anywhere node
	newNode.showDisabled;  // when an anywhere node, does it show if disabled?
	
	// only for anywhere node (activated the anywhere link)
	newNode.activated = false; 
	
	id = (id == undefined) ? genID(nodelist) : id;

	newNode.id = id;
	newNode.anywhere = anywhere;
	
	//nodelist.put(id, newNode);		
	nodelist[id] = newNode;
	
	return id;
}

function createLink(nodeID, start, end, id, type) {
	newLink = new Object();
	newLink.nodeID = nodeID;
	newLink.start = start;
	newLink.end = end;
	//newLink.destnode = destnode
	newLink.followed = 0;
	newLink.rules = [];
	
	//if (id == undefined)
	//	id = genID(linklist);
	id = (id == undefined) ? genID(linklist) : id;
	
	type = (type == undefined) ? "default" : type;
	newLink.type = type;
		
	//var parentnode = nodelist.get(nodeID);
	var parentnode = nodelist[nodeID];
	
	if (parentnode != undefined)
		if ( parentnode != undefined ) {
			addLink(parentnode, newLink);
		}

	//var id = linklist.length;
	newLink.id = id;
	linklist[id] = newLink;
	
	//method
	//newLink.setContent = function(newcontent) {
	//	newLink.content = newcontent;
	//	newLink.end = newLink.start + newcontent.length;
	//}
	
	return id;
}

// name is gotten from the content start to end index on the link
function createChoiceLink(nodeID, start, end, id, type) {
	newLink = new Object();
	newLink.nodeID = nodeID;
	newLink.start = start;
	newLink.end = end;
	//newLink.destnode = destnode
	newLink.followed = 0;
	newLink.rules = [];
	
	// specific to choice link
	newLink.name = nodelist[nodeID].content.substring(start,end);
	
	//if (id == undefined)
	//	id = genID(linklist);
	id = (id == undefined) ? genID(linklist) : id;
	
	type = (type == undefined) ? "default" : type;
		
	//var parentnode = nodelist.get(nodeID);
	var parentnode = nodelist[nodeID];
	
	if (parentnode != undefined)
		if ( parentnode != undefined ) {
			//addLink(parentnode, newLink);
			parentnode.clinks[parentnode.clinks.length] = newLink;
		}

	//var id = linklist.length;
	newLink.id = id;
	linklist[id] = newLink;
	
	//method
	//newLink.setContent = function(newcontent) {
	//	newLink.content = newcontent;
	//	newLink.end = newLink.start + newcontent.length;
	//}
	
	return id;
}

// TODO: useful general function (move out of this file)
function insertSorted( arr, obj, comparator ) {
	// NOTE: termination condition is i<=arr.length, it reaches beyond the last element
	//       and insert in the empty slot at the end if needed
	for (var i=0; i<=arr.length; i++) {
		if ( arr[i] == undefined || ( arr[i] != undefined && comparator( obj, arr[i] ) ) ) {
			insert( arr, i, obj);
			break;
		}
	}
}

function addLink(node, newLink) {
	function comesBefore( link1, link2 ) {
		return link1.start < link2.start;
	}
	insertSorted(node.links, newLink, comesBefore);
}
	
// find a slot in the array that is undefined (not filled) 
// and return the index
function genID(arr) {
	// find the first index that is undefined
	var i=0;
	while (arr[i] != undefined) i++;
	return i;
}

function insert ( links, index, obj) {
	// if slot taken relocate content of this slot first
	if (links[index] != undefined) {
		insert (links, index+1, links[index]);
	}
	// add the obj
	links[index] = obj;
	return links;
}

//	http://www.mojavelinux.com/articles/javascript_hashes.html
function hashtable() {
	this.length = 0;
	this.items = new Array();
	for (var i = 0; i < arguments.length; i += 2) {
		if (typeof(arguments[i + 1]) != 'undefined') {
			this.items[arguments[i]] = arguments[i + 1];
			this.length++;
		}
	}
	
	this.del = function(in_key) {
		var tmp_previous;
		if (typeof(this.items[in_key]) != 'undefined') {
			this.length--;
			var tmp_previous = this.items[in_key];
			delete this.items[in_key];
		}
		return tmp_previous;
	}

	this.get = function(in_key) {
		return this.items[in_key];
	}

	this.put = function(in_key, in_value) {
		var tmp_previous;
		if (typeof(in_value) != 'undefined') {
			if (typeof(this.items[in_key]) == 'undefined') {
				this.length++;
			}
			else {
				tmp_previous = this.items[in_key];
			}
			this.items[in_key] = in_value;
		}
		return tmp_previous;
	}

	this.exist = function(in_key) {
		return typeof(this.items[in_key]) != 'undefined';
	}

	this.clear = function() {
		for (var i in this.items) {
			delete this.items[i];
		}
		this.length = 0;
	}
	return this;
}
// end hide script from old browsers -->