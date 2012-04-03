var start_tag = "<font size='5'>";
var end_tag = "</font>";
//<img src='paper.png' class='stretch' />
//var page_start_tag = "<section><div>";
//var page_end_tag = "</div></section>";

var page_start_tag = "<div name='page'><div name='pagecontent'>"//"<section><div>";
var page_end_tag = "</div></div>";//</section>";
var test_bed_html_cache = "";

// here we need to add anywhere node 
function htmlFormat(content, links, noformat) {
	noformat = (noformat == undefined) ? false : noformat;
	// makes sure test_bed is same dimension as our page_width
	//$("test_bed").style.width = line_break_width;
	
	var alt_offset = -1;
	function offset_to_link( offset ) {
		for ( i in links ) {
			//disp("link start "+ links[i].start);
			//disp("link end "+ links[i].end);
			//disp("offset "+offset);
			if ( (links[i].start <= offset) && (offset < links[i].end) ) {
				return links[i];
			}
		}
	}
	
	// main control loop (entrance)
	function helper2(offset) {
		//disp("helper "+offset);
		if (offset == content.length) return "";
		else {
			var link_find =  offset_to_link(offset);
			//disp("link find typeof "+ link_find);
			if (link_find == undefined) {
				//disp("undefined");
				return plain_text_code2 ( offset);
			} else {
				//disp("found link "+link_find);
				return link_code2 ( offset, link_find);
			}
			//return (link_find == "undefined") ? plain_text_code2 ( offset) : link_code2( offset, link_find );
		}
	}
	
	// TODO i think all the here 2 to here 5 are very similar and can be shrunk
	function plain_text_code2 ( offset, linktext, linklen ) {
	
		//disp("plain text code 2 "+offset+ " " + content.length);
		//disp("linktext "+linktext);
		//disp("alt_offset "+alt_offset);
		
		// if we pass in linktext then we are breaking that link
		var plaintext = (linktext == undefined) ? content.substring(offset, content.length) : linktext.substring(alt_offset, linktext.length);
		//disp ("tset "+ (linktext == undefined));
		//disp("plaintext len "+plaintext.length);
		//disp("plaintext "+plaintext+"|");
		
		//disp("content len "+content.length);
		var space_pos = plaintext.indexOf(" ");
		var nlpos = plaintext.indexOf("\n");
		
		//disp("plaintext "+plaintext);
		
		function linkstart_find ( from_index, to_index ) {
			//disp("links len "+links.length);
			for (i in links) {
				//disp("from_index "+from_index);
				//disp("to_index "+to_index);
				//disp("link start "+ links[i].start);
				if ( links[i].start >= from_index && (links[i].start < to_index || from_index == to_index ) ) { // in special case of from==to index we still match start of link
					//disp("LINK FOUND ");
					return links[i];
				}
			}
		}
		
		var link_find = linkstart_find ( offset, offset+plaintext.length );
		//disp("link find "+link_find.content);
		
		// CASE1 Note in this first case we don't break the linktext/plaintext up since there is no newline and space character
		// however, it is possible a very long string of text is here and this would not be cut up by page break; or rather
		// it will be thrown in a page by itself irregardless of whether it fits or not.
		if ( space_pos == -1 && nlpos == -1 ) {// left one more word (whole of plaintext) (or a very long string without space in it)
			//disp( "case1" );
			//disp("plaintext "+plaintext);
			
			// there could be a link hidden in this string
			//var link_find = linkstart_find ( offset, offset+plaintext.length );
			
			if ( alt_offset == -1 ) {
				//disp("alt_offset here "+plaintext.length);
				return pagebreak_check2 ( start_tag + plaintext + end_tag, offset+plaintext.length );
			} else {

				//disp("alt_offset here2 "+alt_offset);
				//alt_offset += plaintext.length;
				//disp("terminate 2");
				//disp("link_find "+link_find);
				
				// donw with breaking this link text, exit break linktext mode (alt_offset != -1) is the indicator
				alt_offset = -1;
				
				//disp("new offset "+ offset+( link_find.end - link_find.start ));
				return pagebreak_check2 ( start_tag + plaintext.substring( 0, plaintext.length ) + end_tag, offset+(link_find.end - link_find.start) );
			}
			// CASE 2
		} else if (space_pos == 0) {// add that space
			//disp("case 2");
			if (alt_offset == -1)
				return pagebreak_check2( start_tag + " " + end_tag, offset+1);
			else {
				alt_offset++;
				//disp("link leng "+ (link_find.end -  link_find.start));
				//disp("alt_offset after "+alt_offset);
				if (alt_offset == (link_find.end -  link_find.start)) {
					alt_offset = -1;
					return pagebreak_check2( start_tag + " " + end_tag, offset+(link_find.end -  link_find.start)); 
				} else 
					return pagebreak_check2( start_tag + " " + end_tag, offset);
			}
			// CASE 3
		} else if (nlpos == 0) { // add that newline
			//disp("case 3");
			if (alt_offset == -1)  {
				//disp("normal break ");
				return pagebreak_check2( "<br>", offset+1 );
			} else {
				alt_offset++;
				disp("offset here "+offset);
				disp("alt_offset here1 "+alt_offset);
				disp("link find start "+link_find.start);
				disp("link find end "+link_find.end);
				disp("link len "+(link_find.end -  link_find.start));
				if (alt_offset == (link_find.end -  link_find.start)) {
					//disp("link break finish ");
					alt_offset = -1;
					//disp("alt_offset here2 "+alt_offset);
					return pagebreak_check2( "<br>", offset+(link_find.end -  link_find.start)); 
				} else {
					//disp("breaking link newline");
					return pagebreak_check2( "<br>", offset);
				}
			}
			// CASE 4
		} else {  // adding string up to where we found space or newline (a space OR newline found within this string)
			//disp("case 4");
			
			// there could be a link hidden in this string
			var link_find = linkstart_find ( offset, offset+plaintext.length );
			
			var step;
			// note both space_pos and nlpos equal -1 is already not true
			if (space_pos == -1) // only spacepos not found, nlpos found
				step = nlpos;
			else if (nlpos == -1) // only nlpos not found, space_pos found
				step = space_pos;
			else 
				step = (nlpos < space_pos) ? nlpos : space_pos;
			
			//disp("offset in plain text "+offset);
			//disp("link_find "+link_find);
			// if a link comes before any of the delimiting character, break using link start index
			if (link_find != undefined) {
				//disp("link found "+link_find.start);
				// we're breaking links
				if (link_find.start == offset) {
					//disp("we're bvreakihg links");
					if (plaintext.length == 0) { // terminate break link
						//disp("term break link");
						// if link found in the range need to see whether the link is the next break point
						//step = Math.min( step, link_find.start - offset);
						//disp("link content "+ content.substring(link_find.start, link_find.end));
						//disp("step "+step);

						alt_offset = -1;
						//disp("new offset "+ offset+( link_find.end - link_find.start ));
						// jump offset to after the link
						return pagebreak_check2( start_tag + plaintext.substring(0 , step) + end_tag, offset+( link_find.end - link_find.start ));
					} else { // dont increment offset if still breaking link
						//disp("alt_offset incrementing");

						//disp("step "+step);
						//disp("nlpos "+nlpos);
						//disp("space_pos "+space_pos);
						alt_offset += step; // just keep alt_offset corresponding to the linktext (might be alt text or default text)
						return pagebreak_check2( start_tag + plaintext.substring(0, step) + end_tag, offset );
					}
				} else { // still breaking text in front of the link found or gone past the link
					//disp("NOT breaking links");
					// if link found in the range need to see whether the link is the next break point
					step = Math.min( step, link_find.start - offset);
					//disp("link content "+ content.substring(link_find.start, link_find.end));
					//disp("step "+step);
					return pagebreak_check2( start_tag + content.substring(offset, offset + step) + end_tag, offset + step);
				}
				
			} else { // no link found in this node, break as usual
				//disp("breaking AS USUAL");
				return pagebreak_check2( start_tag + content.substring(offset, offset + step) + end_tag, offset + step);
			}
		}
	}
	
	function link_code2 ( offset, link ) {
		//disp("link code 2 "+offset);
		//disp("link "+link);
		//var currLink = links[index];
		var start = link.start;
		var end = link.end;
		var altcontent = findReplaceText(link.id);
		var linktext = (altcontent == undefined) ? content.substring(start, end) : altcontent;
		
		//disp("altcontent "+altcontent);
		//disp("start end "+start+" "+ end);
		//disp("link text "+linktext);
		// NOTE: there is no rules with no actions, 
		// the export process to javascript code will makes sure that's true
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
					/*code = "<div name='button-placeholders' style='height: button_height; margin-top: -10px;' id='" 
						+ escaped_string
						+ "'>"
						+ "button placeholder roar "
						+ linktext
						+ "</div>";
						*/
					
					// debug
					// need to be overflow: scroll to ensure div has button_height, else it just scale to fit the content
					// overflow: scroll;
					//button-height height
					
					//sigh why can't i use button height here
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
					
					//code = "<div style='height: button_height; margin-top: -15px;'><img src=paper.png width=300 height=50></img></div>";
					//break;
			}
			
			return pagebreak_check2( code, end );
			
			// if popup, the follow action of the link should have black_fade_in in place of gotoNode
			
		} else if ( link_clickable ( link, false ) && !noformat ) {
			var code = "<b>"
				+ start_tag
				+ linktext
				+ end_tag
				+ "</b>";
			return pagebreak_check2( code, end );
				//+"</b>" + helper(index+1, end);

		} else if (!noformat) {  // just plain text
			// offset is same as start for a link;
			//disp("start "+start);
			//disp("end "+end);
			//disp("linktext "+linktext);
			if (alt_offset == -1) {// init break link
				//disp("setting alt offset 0");
				alt_offset = 0;
			}
			return plain_text_code2( offset, linktext );
		} else if (noformat) {
			//return plain_text_code2( offset, linktext );
			return pagebreak_check2( start_tag + linktext + end_tag, end );
		}
	}
	
	function pagebreak_check2 ( htmlcode, new_offset ) {
		
		if (!noformat) {
			// make sure test_bed has same styling as actual page
			$('test_bed').className = "pagesdiv";
			test_bed_html_cache += htmlcode;
			$("test_bed").innerHTML = "<div name='testcontent'>" + test_bed_html_cache + "</div>";
			var pages = document.getElementsByName("testcontent");
			for (j in pages) {
				pages[j].className = 'pagecontent';
			}
			
			//disp("test bed "+ $("test_bed").clientHeight + " " + btm_height);
			//disp("htmlcode "+ $("test_bed").innerHTML);
			
			if ($("test_bed").clientHeight < text_area_height) {
				return htmlcode + helper2(new_offset);
			} else {
				$("test_bed").innerHTML = "";
				test_bed_html_cache = "";
				//disp("page break else");
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
	
	//alert("setting inner HTML to empty2 " + $("test_bed").clientHeight);
	
	
	if (noformat)
		return htmlcode ;
	else
		//return page_start_tag + htmlcode + page_end_tag + "<br><text>WTF</text><br>" + anywherelink_htmlcode();
		return  page_start_tag +  htmlcode + page_end_tag;
}

function escape_special( str ) {
	//disp("escape_special "+str);
	var result = "";
	result = str.substring(0, 1);
	if (result == '"') {
		//disp("DOUBLE QUOTE FOUND");
		result = "&quot;";
	}
	if (result == "'") {
		//disp("SINGLE QUOTE FOUND");
		result = "&#39;";
	}
	if (result == "\\") 
	{
		result = "&apos;";
	}
	if (str.length == 0)
		return "";
	else 
		return result + escape_special( str.substring (1, str.length) );
}

// propose new design.
// pass arr of arrs representing pages.
// replace alt text ...