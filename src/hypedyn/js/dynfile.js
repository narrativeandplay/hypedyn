function loadStory() {
	setStartNode(2);
	createNode("start", "This is the story of a little girl named Little Red Riding Hood, which she was called because of the red hood that she often wore.\n\nOne day she was walking through the forest.", false, 2);
	createLink(2, 101, 109, 151);
		createRule(151, "link", "if", "and", true, 159);
			createCondition(nodeVisited, 3, 159, false, 162);
			createAction("clickedLink", 159, gotoNode, [144], 161);
		createRule(151, "link", "not", "and", true, 160);
			createCondition(nodeVisited, 3, 160, false, 163);
	createLink(2, 168, 174, 145);
		createRule(145, "link", "if", "and", true, 170);
			createAction("clickedLink", 170, gotoNode, [4], 172);
		createRule(145, "link", "not", "and", true, 171);
		createRule(2, "node", "if", "and", true, 176);

	createNode("end", "*** The End ***\n\nback to start", false, 3);
	createLink(3, 25, 30, 149);
		createRule(149, "link", "if", "and", true, 164);
			createAction("clickedLink", 164, gotoNode, [2], 166);
		createRule(149, "link", "not", "and", true, 165);
		createRule(3, "node", "if", "and", true, 175);

	createNode("forest", "In the forest, Red came across a young man with a nasty smile.\n\n\"Where are you going, little girl?\" he asked.\n\n\"I'm off to see my sick granny,\" she said.\n\nWell, you can probably guess what happened next.", false, 4);
	createLink(4, 198, 202, 147);
		createRule(147, "link", "if", "and", true, 167);
			createAction("clickedLink", 167, gotoNode, [3], 169);
		createRule(147, "link", "not", "and", true, 168);
		createRule(4, "node", "if", "and", true, 174);

	createNode("Hood details", "Her hood was a magic garment, given to her by her grandmother. It could kill anyone who tried to harm the wearer.\n\nBut not immediately. And in a most painful manner.\n\nMeanwhile, in the forest...", false, 144);
	createLink(144, 185, 191, 154);
		createRule(154, "link", "if", "and", true, 156);
			createAction("clickedLink", 156, gotoNode, [4], 158);
		createRule(154, "link", "not", "and", true, 157);
		createRule(144, "node", "if", "and", true, 173);

}