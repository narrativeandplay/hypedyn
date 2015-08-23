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
 
var start_tag = "<font class='plaintext'>"; // used to be size=5 inside font tag
var end_tag = "</font>";

var page_start_tag = "<div name='page'><div name='pagecontent'>"//"<section><div>";
var plain_page_start_tag = "<div name='plainpage'><div name='plainpagecontent'>"// for plain pages
var page_end_tag = "</div></div>";//</section>";
var test_bed_html_cache = "";

var dormant_start_tag_not_visited = "<font class='dormant_not_visited'>";
var dormant_end_tag_not_visited = "</font>"

var dormant_start_tag_visited = "<font class='dormant_visited'>";
var dormant_end_tag_visited = "</font>"

// special characters in the string that is used by html syntax
// need to be encoded to show appropriately
function escape_special( str ) {

   var offset = 0;
   function replace_all (str, match_str, replace_str, replace_count) {
		var find = str.indexOf(match_str);
		if ( find == -1 ) {
			offset += (replace_str.length - match_str.length) * replace_count;
			return str;
		} else {
			return replace_all( str.substring(0, find) + replace_str + str.substring( find + match_str.length, str.length ), match_str, replace_str, replace_count+1 );
		}
   }
   
   //str = replace_all(str, '"', "&quot;", 0);
   //str = replace_all(str, "'", "&#39;", 0);
   //alert("offset now "+offset);
   //str = replace_all(str, "\\", "&apos;", 0);
   str = replace_all(str, "<", "&lt;", 0);
   str = replace_all(str, ">", "&gt;", 0);
   
   // function helper (str) {
		// var result = "";
		// result = str.substring(0, 1);
		// if (result == '"') {
			// result = "&quot;";
		// } if (result == "'") {
			// result = "&#39;";
		// } if (result == "\\") {
			// result = "&apos;";
		// } if (result == "<") {
			// result = "&lt;";
		// } if (result == ">") {
			// result = "&gt;";
		// } if (str == "") {
			// result = "";
		// } else {
			// return result + helper( str.substring (1, str.length), offset );
		// }
	// }
	
	//return [ str, offset ];
	return str;
}

function node_to_html( node, activated_anywhere_nodes, inactive_anywhere_nodes, plain_only ) {
	var node_content = node.content;
	var links = clone_arr( node.links );
	
	function offset_to_link( offset ) {
		for ( i in links ) {
			//disp("otl "+links[i].start+" "+links[i].end);
			if ( (links[i].start <= offset) && (offset < links[i].end) ) {
				return links[i];
			}
		}
	}
	
	function next_link_from( offset ) {
		for ( i in links ) {
			if ( links[i].start >= offset ) {
				return links[i];
			}
		}
	}
	
	function next_link_index_from( offset ) {
		link = next_link_from( offset );
		if ( link == undefined ) return undefined;
		else return link.start;
	}
	
	function generate_tag_groups( content ) {
		return generate_tag_groups_helper( content, 0, 0, [] );
	}
	
	// split between linktext and plaintext
	// also do the alt text substitution here
	// [Tag Group] are how we would divide the content into different tags 
	// in html code anyway. It contains the text content, link and linkid if it is associated
	// with a link, type whether it is "plain" or "link".
	function generate_tag_groups_helper( content, offset, tag_index, retval ) {
		
		var link_index = next_link_index_from( offset );
		
		if ( link_index == undefined ) {// no more links beyond here
			var new_tag_group = [];
			new_tag_group.content = content.substring( offset, content.length );
			//disp('plaintext '+new_tag_group.content);
			//alert("plaintext");
			new_tag_group.content = escape_special( new_tag_group.content );
			new_tag_group.type = "plain";
			retval.push( new_tag_group );
			return retval;
		}
		else 
		if ( link_index == offset ) { // its a link
			var new_tag_group = [];
			var link = next_link_from( offset );
			
			// replacing link text with alt text if it exists
			var altcontent = findReplaceText(link.id);
			
			var linktext = (altcontent == undefined) ? content.substring(link.start, link.end) : altcontent;

			linktext = escape_special( linktext );
			var offset_inc = link.end - link.start;
			
			new_tag_group.content = linktext; 
			new_tag_group.type = "link";
			new_tag_group.linkid = link.id;
			new_tag_group.link = link;
			retval.push( new_tag_group );
			
			return generate_tag_groups_helper ( content, offset + offset_inc, tag_index+1, retval );
		} else if ( link_index > offset ) { // plain text
			var new_tag_group = [];
			new_tag_group.content = content.substring( offset, link_index );
			new_tag_group.type = "plain";
			//disp('plaintext '+new_tag_group.content);
			//alert("plaintext");
			new_tag_group.content = escape_special( new_tag_group.content );
			retval.push( new_tag_group );
			var offset_inc = link_index - offset;
			return generate_tag_groups_helper ( content, offset + offset_inc, tag_index+1, retval );
		} else {
			//disp("offset "+offset);
			//disp("link index "+link_index);
			alert(" error state in generate_tag_groups ");
		}
	}
	
	// generate the actual html start and end tag
	function assign_html_tag( tg_arr ) {
		for ( var i in tg_arr ) {
			var tag = tg_arr[i]; // tg stands for tag group
			if ( tag.type == "plain") {
				//disp("plain text tag added");
				tag.start_tag = start_tag;
				tag.end_tag = end_tag;
			} else if (tag.type == "link") {
				//disp("link tag added");
				var link = tag.link;
				var noformat = plain_only;
				
				if ( link_clickable ( link, true ) && !noformat ) {
					switch( link.type ) {
						case "default": 
							tag.start_tag = "<a href='javascript:void(0)' "
                                            // set link to followed or normal: note that link is generated so the usual followed state doesn't work
                                            + (link.followed ? "class='intextlink-followed' " : "class='intextlink' ")
											+ "onMouseUp='"
											+ "clickedLink(" + link.id + ")'>";
							tag.end_tag = "</a>";
							break;
						case "choice":
							tag.start_tag = "<div name='button-placeholders' style='height: 50px; margin-top: -10px; background-color: green' id='" 
											+ tag.content //escaped_string
											+ "'>" 
											+ "<a href='javascript:void(0)' "
											+ "onMouseUp='"
											+ "clickedLink(" + link.id + ")'>";
							tag.end_tag = "</a></div>";
							break;
					}
				// Note different from java version
				// this check whether the link has been followed as opposed to whether the destination node has been visited
				} else if ( link_clickable ( link, false ) && !noformat) {
					if  ( !link.followed ) {
						tag.start_tag = dormant_start_tag_not_visited;
						tag.end_tag = dormant_end_tag_not_visited;
					} else {
						tag.start_tag = dormant_start_tag_visited;
						tag.end_tag = dormant_end_tag_visited;
					}
				} else if (!noformat) {  // just normal plain text
					tag.start_tag = start_tag;
					tag.end_tag = end_tag;
				} else if (noformat) { //  probably popup node
					tag.start_tag = start_tag;
					tag.end_tag = end_tag;
				} else {
					alert("Error state in assign_html_tag");
				}
			}
		}
		return tg_arr;
	} // end of assign_html_tag

	// include inactive anywhere nodes
	function generate_anywhere_tg ( active_anywhere_nodes, inactive_anywhere_nodes ) {
		var anywhere_tg_arr = [];
		
		// double <br>
		var br_tg = [];
		br_tg.content = "";
		br_tg.start_tag = "<br><br>"
		br_tg.end_tag = "";
		anywhere_tg_arr.push( br_tg );
		
		disp("active anywhere nodes "+active_anywhere_nodes);
		disp("len "+ active_anywhere_nodes.length);
		disp("null? "+ ( active_anywhere_nodes === null));
		
		disp("inactive anywhere nodes "+inactive_anywhere_nodes);
		disp("len "+ inactive_anywhere_nodes.length);
		disp("null? "+ ( inactive_anywhere_nodes === null));
		
		// active
		for ( var j in active_anywhere_nodes ) {
			var anywherenode = active_anywhere_nodes[j];
			disp("anywhere node "+anywherenode);
			var anywhere_tg = [];
            // need to escape the content in case name contains special characters
			anywhere_tg.content = escape_special(anywherenode.label); //escape_special(anywherenode.name);
			anywhere_tg.start_tag = "<a href='javascript:void(0)' class='anywhere' onMouseUp='clickedLink(" 
									+ anywherenode.id + ")'>";
			anywhere_tg.end_tag = "</a><br>";
			anywhere_tg_arr.push( anywhere_tg );
		}
		
		// inactive
		for ( var j in inactive_anywhere_nodes ) {
			var anywherenode = inactive_anywhere_nodes[j];
			disp("anywhere node "+anywherenode);
			var anywhere_tg = [];
            // need to escape the content in case name contains special characters
			anywhere_tg.content = escape_special(anywherenode.label); //escape_special(anywherenode.name);
			anywhere_tg.start_tag = "<b class='anywhere'>";
			anywhere_tg.end_tag = "</b><br>";
			anywhere_tg_arr.push( anywhere_tg );
		}
		return anywhere_tg_arr;
	}
	
	//
	function page_break_tg( tg_arr ) {
		
		function format_test_bed() {
			var pages = document.getElementsByName("testcontent");
			for (j in pages) {
				if (isNumber(j)) {
					//pages[j].style.height = text_area_height;
					pages[j].className = 'pagecontent';
					pages[j].style.width = (page_width - (2*20)); // padding
				}
			}
		}
		
		function reset_test_bed() {
			$("test_bed").innerHTML = "";
			test_bed_html_cache = "";
		}
		
		// find which index of the tg when added overflows to next page
		function index_that_overflow ( tg_arr, index ) {
			//disp("index that overflow here " + index);
			if ( index == tg_arr.length ) { // terminating condition
				reset_test_bed();
				return -1; // we've hit the end and this tg does not need to be broken
			} else if ( index <= tg_arr.length - 1 ) {
				test_bed_html_cache += assemble_tg( tg_arr[index] );
				$("test_bed").innerHTML = "<div name='testcontent'>" + test_bed_html_cache + "</div>";
				format_test_bed();
				//print_tg( tg_arr );
				//disp("tg_arr.length "+tg_arr.length);
				//disp("index "+index);
				
				//alert("test bed height "+$("test_bed").clientHeight);
				if ($("test_bed").clientHeight < text_area_height) {
					var next_index = index + 1;
					//alert( "next iter "+next_index );
					return index_that_overflow ( tg_arr, next_index );
				} else {
					//alert("hit the right spot "+index);
					reset_test_bed();
					return index;
				}
			} else {
				alert("ELSE index_that_overflow error state");
			}
		}
		
		// breaks a tg's content into 2 at the nth space delimiter 
		function break_tg ( tg, n_index ) {
			//alert("break_tg index "+n_index)
			var content = tg.content;
			var words = content.split(" ");
			//disp("calling subarray from break_tg "+ n_index);
			var front_words = subarray( words, 0, n_index ); //words.splice( 0, n_index );
			var back_words = subarray( words, n_index, words.length ); //words.splice( n_index, words.length );
			var front_content = front_words.join(" ");
			var back_content = back_words.join(" ");
			
			var front_tg = [];
			var back_tg = [];
			
			front_tg.content = front_content;
			front_tg.start_tag = tg.start_tag;
			front_tg.end_tag = tg.end_tag;
			front_tg.type = tg.type;
			
			back_tg.content = back_content;
			back_tg.start_tag = tg.start_tag;
			back_tg.end_tag = tg.end_tag;
			back_tg.type = tg.type
			
			return [ front_tg, back_tg ];
		}
		
		// incrementally add portions of tg_to_break until the page overflows
		// at that point return the word index so that it can be used to cut tg_to_break
		function adjust_break_point ( tg_to_break, front_tg_arr ) {
			//alert("START adjust break pt ");
			function helper ( delim_index ) {
				//disp("helper "+delim_index);
				var broken_tg = break_tg( tg_to_break, delim_index );
				var tg_to_break_front = broken_tg[0];
				var tg_to_break_back = broken_tg[1];

				var overflow_test = index_that_overflow ( front_tg_arr.concat( [ tg_to_break_front ] ) , 0 );
				if ( overflow_test >= 0 || tg_to_break_back.content.length == 0) {
					return delim_index;
				} else {
					return helper ( delim_index + 1 );
				}
			}
			return helper( 1 );
		}
		
		function arr_append ( arr1, arr2 ) {
			return arr1.concat( arr2 );
		}

		var break_index = index_that_overflow ( tg_arr, 0 );
		if ( break_index >= 0 ) { // means we need to break it
			var front_tg_arr = subarray( tg_arr, 0, break_index );//.splice(0, break_index);
			var tg_to_break = tg_arr[break_index];
			var back_tg_arr = subarray( tg_arr, break_index+1, tg_arr.length );//clone_arr(tg_arr).splice(break_index+1, tg_arr.length);
			
			var delim_index = adjust_break_point( tg_to_break, front_tg_arr ); // start with 1 (first word);

			var broken_tg = break_tg( tg_to_break, delim_index - 1 ); // since delim_index is the first one to overflow dont include
			var tg_to_break_front = broken_tg[0];
			var tg_to_break_back = broken_tg[1];
			
			// every element in the return array is an array of tag groups that fill up a page
			return arr_append ( [ front_tg_arr.concat( [ tg_to_break_front ]) ], page_break_tg( [tg_to_break_back].concat( back_tg_arr ) ) ) ;
			//return front_tg_arr.concat(tg_to_break_front);
		} else {
			//alert("not breaking ");
			return [ tg_arr ];
		}
	}
	
	function assemble_tg ( tg ) {
		//disp("[assemble] "+tg.content);
		return tg.start_tag + tg.content + tg.end_tag;
	}
	
	function assemble_html_code( pages_tg_arr, plain_only ) {
        var retval = "";
        // need to make sure plain pages are tagged differently so they don't get styled
        var actual_page_start_tag = plain_only ? plain_page_start_tag : page_start_tag;
		for ( var i in pages_tg_arr ) {
			if ( isNumber(i) ) {
				var curr_page_tg_arr = pages_tg_arr[i];
				var curr_page_code = "";

                // process the newlines - is there a reason why this isn't done in escape_special?
                //process_newline(curr_page_tg_arr); moved out before page_break_tg - alex

				//disp(" curr_page_tg_arr len "+curr_page_tg_arr.length);
				for ( var j in curr_page_tg_arr ) {
					curr_page_code += assemble_tg( curr_page_tg_arr[j] );
					//disp("curr_page_code "+curr_page_code);
				}
				//disp("[curr page code]! "+curr_page_code);
				retval += actual_page_start_tag + curr_page_code + page_end_tag;
			}
		}

		return retval;
	}
	
    // replace \n with <br>
	function process_newline( tg_arr ) {
	
		function clone_arr_obj( obj ) {
			var retval = [];
			for ( var prop in obj ) {
				//disp("cloning "+prop);
				//disp("obj prop "+obj[prop]);
				retval[prop] = obj[prop];
			}
			return retval;
		}
		
		for ( var i in tg_arr ) {
			curr = tg_arr[i];
			var content = tg_arr[i].content;
			newline_index = content.indexOf("\n");
			if ( newline_index != -1 ) {
				//disp(" [newline replace!] ")
				var new_tg = clone_arr_obj( curr );
				//print_arr( new_tg );
				curr.content = content.substring( 0, newline_index );
				curr.end_tag += "<br>";
				new_tg.content = content.substring ( newline_index+1, content.length );

				var new_index = i;
				tg_arr.splice( parseInt(i)+1, 0, new_tg );
				process_newline( tg_arr );
			}
		}
		return tg_arr;
	}
	
	// break the content into segmented by links and non links
	var tg_arr = generate_tag_groups( node_content );

	// decide which tags to use for links depending on whether it is clickable, dormant etc
	// plain text is assigned tag for plaintext
	tg_arr = assign_html_tag( tg_arr );

	// anywhere nodes
	var anywhere_tg_arr = generate_anywhere_tg( activated_anywhere_nodes, inactive_anywhere_nodes );
	tg_arr = tg_arr.concat( anywhere_tg_arr );
    
    // process newlines
    process_newline(tg_arr);
    
	// dont try to break page when page flipping mode off
	// pages_tg_arr is an array of tg_arr
	var pages_tg_arr = (!plain_only && page_flipping_mode) ? page_break_tg( tg_arr ) : [ tg_arr ];
	
	text_to_replace = []; // clear after use
	
	//disp("how many page? "+pages_tg_arr.length);
	for (var i in pages_tg_arr ) {
		print_tg( pages_tg_arr[i] );
	}

	var htmlcode = assemble_html_code( pages_tg_arr, plain_only );
	return htmlcode;
}

function print_tg( tg_arr ) {

}

function print_tg_arr( arr ) {
	for( var i in arr ) {
		print_tg( arr[i] );
	}
}

function subarray( arr, start_index, end_index ) {
	//disp("subarray "+start_index + " " + end_index);
	var retval = clone_arr(arr);
	retval = retval.splice( start_index, (end_index - start_index));
	return retval;
}