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
 
var start_tag = "<font class='plaintext'>"; // used to be size=5 inside font tag
var end_tag = "</font>";

var page_start_tag = "<div name='page'><div name='pagecontent'>"//"<section><div>";
var page_end_tag = "</div></div>";//</section>";
var test_bed_html_cache = "";

var dormant_start_tag = "<font class='dormant'>";
var dormant_end_tag = "</font>"

// given the content(text) of the node, and the arr of links
// this returns the html code to display the node's content and the links
// noformat flag stops all page breaking and just return the plain text
function htmlFormat(content, links, noformat) {
	noformat = (noformat == undefined) ? false : noformat;
	
	var alt_offset = -1;

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
	
	// main control loop (entrance)
	function helper2(offset) {
		if (offset == content.length) {
			if (links.length != 0)
				// assume the rest in links are anywhere nodes
				return anywherelink_code ( links[0], offset );
			else return "";
		}
		else {
			var link_find = offset_to_link( offset );
			if ( link_find == undefined ) {
				return plain_text_code2 ( offset );
			} else {
				return link_code2 ( offset, link_find );
			}
		}
	}
	
	function plain_text_code2 ( offset, linktext, link ) {

		// if we pass in linktext then we are breaking that link
		var plaintext = (linktext == undefined) ? content.substring(offset, content.length) : linktext.substring(alt_offset, linktext.length);
		
		var space_pos = plaintext.indexOf(" ");
		var nlpos = plaintext.indexOf("\n");
		
		var next_link = next_link_from( offset );
		var next_link_pos = (next_link == undefined) ? -1 : next_link.start - offset;

		// mode transition between breaking link text and normal text
		if (linktext != undefined) {
			if (alt_offset == -1) {// init break link
				alt_offset = 0;
			}
		}
		
		if (alt_offset == -1) { // not breaking link text
			var newcode;
			var offset_inc;
			if ( space_pos == -1 && nlpos == -1 && next_link_pos == -1 ) {
				newcode = start_tag + plaintext + end_tag;
				offset_inc = plaintext.length;
			} else if ( space_pos == 0 ) {
				newcode = start_tag + " " + end_tag;
				offset_inc = 1;
			} else if ( nlpos == 0 ) {
				newcode = "<br>";
				offset_inc = 1;
			} else {
				// take the smallest of the 3, ignore those that are negative
				var arr = [];
				if ( space_pos > 0 ) {
					arr[arr.length] = space_pos;
				} 
				if ( nlpos > 0 ) {
					arr[arr.length] = nlpos;
				}
				if ( next_link_pos > 0 ) {
					arr[arr.length] = next_link_pos;
				}
				offset_inc = Math.min.apply( this, arr );
					
				newcode = start_tag + plaintext.substring(0, offset_inc) + end_tag;
			}
			return pagebreak_check2( newcode, offset + offset_inc );
			
		} else { // breaking link text
			linktext_len = linktext.length;
			var newcode;
			var offset_inc;
			
			if ( space_pos == -1 && nlpos == -1 ) {
				newcode = start_tag + plaintext.substring( 0, plaintext.length ) + end_tag;
				alt_offset += plaintext.length;
			} else if ( space_pos == 0 ) {
				alt_offset++;
				newcode = start_tag + " " + end_tag;
			} else if ( nlpos == 0 ) {
				alt_offset++;
				newcode = "<br>";
			} else {
				var step;
				// note both space_pos and nlpos equal -1 is already not true
				if (space_pos == -1) // only spacepos not found, nlpos found
					step = nlpos;
				else if (nlpos == -1) // only nlpos not found, space_pos found
					step = space_pos;
				else 
					step = Math.min( nlpos, space_pos );
					
				alt_offset += step; // just keep alt_offset corresponding to the linktext (might be alt text or default text)
				newcode = start_tag + plaintext.substring( 0, step ) + end_tag;
			}
			
			// terminating condition for breaking link (reached the end of the string)
			if ( alt_offset == linktext_len ) { //(alt_offset == (link_find.end -  link_find.start))
				offset_inc = link.end - link.start;
				alt_offset = -1;
				links.splice(links.indexOf(link), 1);
			} else // do not increment offset (we're in limbo breaking alttext of link that isn't part of the node content)
				offset_inc = 0;
			
			return pagebreak_check2( newcode, offset + offset_inc );
		}
	}
	
	function anywherelink_code ( anywherenode, offset ) {
	//disp("anyywhere processing ");
	//disp("anywherenode id "+anywherenode.id);
	//disp("name "+ anywherenode.name);
		var code = "";
		if (links.length == activated_anywhere_nodes.length) // start of anywhere node link
			code += "<br><br>";
		code += //start_tag +
				"<a href='javascript:void(0)' class='anywhere' onMouseUp='gotoNode(" 
				+ anywherenode.id + ")'>"
				+ anywherenode.name + "</a>"
				//+ end_tag
				+ "<br>";
		links.splice(links.indexOf(anywherenode), 1);
		return pagebreak_check2( code, offset );
	}
	
	function link_code2 ( offset, link ) {
		var start = link.start;
		var end = link.end;
		var altcontent = findReplaceText(link.id);
		var linktext = (altcontent == undefined) ? content.substring(start, end) : altcontent;
		disp("linktext "+linktext);
		disp("altcontent "+altcontent);
		
		// note export process does not ensure that rules have actions
		// therefore we have to check whether a link is clickable by checking the actions in its rules
		if ( link_clickable ( link, true ) && !noformat ) {
			var code ="";
			switch (link.type) {
				case "default":
					code = "<a href='javascript:void(0)' class='intextlink' "
						+ "onMouseUp='"
						+ "clickedLink(" + link.id + ")'>"
						//+ start_tag
						+ linktext
						//+ end_tag
						+ "</a>";
					break;
					//	+"</a>" + helper(index+1, end);
				case "choice":
					// important for choice link to linktext in name
					link.name = linktext;
					
					cache_choice_link( link );
					var escaped_string = escape_special( linktext );
					// negative margin to conteract that introduced by div elements

					code = "<div name='button-placeholders' style='height: 50px; margin-top: -10px; background-color: green' id='" 
						+ escaped_string
						+ "'>" 
						+ "<a href='javascript:void(0)' "
						+ "onMouseUp='"
						+ "clickedLink(" + link.id + ")'>"
						//+ start_tag
						+ linktext
						//+ end_tag
						+ "</a>"
						+ "</div>";
					break;
			}
			// remove link from links array
			links.splice(links.indexOf(link), 1);
			return pagebreak_check2( code, end );
			
		} else if ( link_clickable ( link, false ) && !noformat ) {
			var code = //"<b>"
				dormant_start_tag
				+ linktext
				+ dormant_end_tag;
				//+ "</b>";
			// remove link from links array
			links.splice(links.indexOf(link), 1);
			return pagebreak_check2( code, end );
		} else if (!noformat) {  // just plain text
			disp("linktext "+linktext);
			return plain_text_code2( offset, linktext, link );
		} else if (noformat) {
			// remove link from links array
			disp("linktext "+linktext);
			links.splice(links.indexOf(link), 1);
			return pagebreak_check2( start_tag + linktext + end_tag, end );
		}
	}
	
	function pagebreak_check2 ( htmlcode, new_offset ) {
		if (!noformat && page_flipping_mode) { // dont try to break page when page flipping mode off
			test_bed_html_cache += htmlcode;
			// make sure test_bed has same styling as actual page
			$('test_bed').className = "pagesdiv";
			$('test_bed').style.width = page_width;
			$("test_bed").innerHTML = "<div name='testcontent'>" + test_bed_html_cache + "</div>";
			var pages = document.getElementsByName("testcontent");
			
			for (j in pages) {
				if (isNumber(j)) {
					//disp("j "+j);
					pages[j].className = 'pagecontent';
					//disp("setting page content width1 "+page_width);
					//disp("pages j "+pages[j]);
					//if (pages[j].style)
					pages[j].style.width = (page_width - (2*20)); // padding
				}
			}
			
			if ($("test_bed").clientHeight < text_area_height) {
				return htmlcode + helper2(new_offset);
			} else {
				$("test_bed").innerHTML = "";
				test_bed_html_cache = "";
				disp("breaking page!");
				// remaining code goes to next page
				return page_end_tag + page_start_tag + htmlcode + helper2(new_offset);
			}
		} else {
		// dont break page when not formatting
			return htmlcode + helper2(new_offset);
		}
	}
	
	$("test_bed").innerHTML = "";
	test_bed_html_cache = "";
	
	var htmlcode = helper2(0);
	
	text_to_replace = []; // clear after use
	$("test_bed").innerHTML = ""; // clear after (so the duplicate elements wont be confused with the real ones)
	
	if (noformat)
		return htmlcode ;
	else
		//return page_start_tag + htmlcode + page_end_tag + anywherelink_htmlcode();
		//return  page_start_tag +  htmlcode + "<br><br>" + start_tag + anywherelink_buttons() + end_tag + page_end_tag;
		return  page_start_tag +  htmlcode + page_end_tag;
}

// special characters in the string that is used by html syntax
// need to be encoded to show appropriately
function escape_special( str ) {
	var result = "";
	result = str.substring(0, 1);
	if (result == '"')
		result = "&quot;";
	if (result == "'")
		result = "&#39;";
	if (result == "\\") 
		result = "&apos;";
	if (str == "")
		return "";
	else 
		return result + escape_special( str.substring (1, str.length) );
}

// propose new design.
// pass arr of arrs representing pages.
// replace alt text ...