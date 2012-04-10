var start_tag = "<font size='5'>";
var end_tag = "</font>";

var page_start_tag = "<div name='page'><div name='pagecontent'>"//"<section><div>";
var page_end_tag = "</div></div>";//</section>";
var test_bed_html_cache = "";

// given the content(text) of the node, and the arr of links
// this returns the html code to display the node's content and the links
function htmlFormat(content, links, noformat) {
	noformat = (noformat == undefined) ? false : noformat;
	
	var alt_offset = -1;

	function offset_to_link( offset ) {
		disp("offset to link "+offset);
		disp("links len "+links.length);
		
		for ( i in links ) {
			if ( (links[i].start <= offset) && (offset < links[i].end) ) {
				disp("link id "+links[i].id);
				if ( currNodeID == 119 ) alert("");
				return links[i];
			}
		}
	}
	
	// main control loop (entrance)
	function helper2(offset) {
		//disp("helper "+offset);
		if (offset == content.length) return "";
		else {
			var link_find =  offset_to_link( offset );
			//disp("link find typeof "+ link_find);
			if ( link_find == undefined ) {
				//disp("undefined");
				return plain_text_code2 ( offset );
			} else {
				//disp("found link "+link_find);
				return link_code2 ( offset, link_find );
			}
		}
	}
	
	// TODO i think all the here 2 to here 5 are very similar and can be shrunk
	function plain_text_code2 ( offset, linktext, link ) {

		// if we pass in linktext then we are breaking that link
		var plaintext = (linktext == undefined) ? content.substring(offset, content.length) : linktext.substring(alt_offset, linktext.length);
		
		// check whether a link starts within this range
		function linkstart_find ( from_index, to_index ) {
			//disp("links len "+links.length);
			for (i in links) {
				if ( links[i].start >= from_index && (links[i].start < to_index || from_index == to_index ) ) { // in special case of from==to index we still match start of link
					//disp("LINK FOUND ");
					return links[i];
				}
			}
		}
		
		var link_find = linkstart_find ( offset, offset+plaintext.length );
		
		var space_pos = plaintext.indexOf(" ");
		var nlpos = plaintext.indexOf("\n");
		
		// rewriting the above
		
		// mode transition between breaking link text and normal text
		if (linktext != undefined) {
			if (alt_offset == -1) {// init break link
				alt_offset = 0;
			}
		}
		
		if (alt_offset == -1)  { // not breaking link text
			var newcode;
			var offset_inc;
			if ( space_pos == -1 && nlpos == -1 ) {
				newcode = start_tag + plaintext + end_tag;
				offset_inc = plaintext.length;
			} else if ( space_pos == 0 ) {
				newcode = start_tag + " " + end_tag;
				offset_inc = 1;
			} else if ( nlpos == 0 ) {
				newcode = "<br>";
				offset_inc = 1;
			} else {
				// note both space_pos and nlpos equal -1 is already not true
				if (space_pos == -1) // only spacepos not found, nlpos found
					offset_inc = nlpos;
				else if (nlpos == -1) // only nlpos not found, space_pos found
					offset_inc = space_pos;
				else 
					offset_inc = Math.min( nlpos, space_pos );
					
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
	
	function link_code2 ( offset, link ) {
		var start = link.start;
		var end = link.end;
		var altcontent = findReplaceText(link.id);
		var linktext = (altcontent == undefined) ? content.substring(start, end) : altcontent;
		
		// note export process does not ensure that rules have actions
		// therefore we have to check whether a link is clickable by checking the actions in its rules
		if ( link_clickable ( link, true ) && !noformat ) {
			var code ="";
			switch (link.type) {
				case "default":
					code = "<a href='javascript:void(0)' "
						+ "onMouseUp='"
						+ "clickedLink(" + link.id + ")'>"
						+ start_tag
						+ linktext
						+ end_tag
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
						+ start_tag
						+ linktext
						+ end_tag
						+ "</a>"
						+ "</div>";
					break;
			}
			// remove link from links array
			links.splice(links.indexOf(link), 1);
			return pagebreak_check2( code, end );
			
		} else if ( link_clickable ( link, false ) && !noformat ) {
			var code = "<b>"
				+ start_tag
				+ linktext
				+ end_tag
				+ "</b>";
			// remove link from links array
			links.splice(links.indexOf(link), 1);
			return pagebreak_check2( code, end );
		} else if (!noformat) {  // just plain text
			return plain_text_code2( offset, linktext, link );
		} else if (noformat) {
			// remove link from links array
			links.splice(links.indexOf(link), 1);
			return pagebreak_check2( start_tag + linktext + end_tag, end );
		}
	}
	
	function pagebreak_check2 ( htmlcode, new_offset ) {
		if (!noformat) {
			test_bed_html_cache += htmlcode;
			// make sure test_bed has same styling as actual page
			$('test_bed').className = "pagesdiv";
			$("test_bed").innerHTML = "<div name='testcontent'>" + test_bed_html_cache + "</div>";
			var pages = document.getElementsByName("testcontent");
			
			for (j in pages) {
				pages[j].className = 'pagecontent';
			}
			
			if ($("test_bed").clientHeight < text_area_height) {
				return htmlcode + helper2(new_offset);
			} else {
				$("test_bed").innerHTML = "";
				test_bed_html_cache = "";
				
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
		//return page_start_tag + htmlcode + page_end_tag + "<br><text>WTF</text><br>" + anywherelink_htmlcode();
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