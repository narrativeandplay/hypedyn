function loadStory() {
	setStartNode(1);
	createNode("start", "One day, Little Red Riding Hood is walking through the forest, on the way to deliver a basket of food and flowers to her grandmother.", false, 1);

	createNode("Explore the forest", "Tempted by a grove of flowers, Red strays off the path into the forest. There are a number of different types of flowers growing in the grassy clearing.", true, 3);
		createRule(3, "node", "if", "and", true, 485);
			createCondition(nodeVisited, 9, 485, true, 490);
			createCondition(nodeVisited, 4, 485, true, 489);
			createCondition(nodeVisited, 3, 485, true, 488);
			createAction("anywhereCheck", 485, addAnywhereLink, [3], 486);
			createAction("enteredNode", 485, setFact, [378, true], 487);

	createNode("Go directly to Grandma's house", "Red walks along the path, sticking carefully to the center to avoid the dark, menacing trees. Eventually, she reaches Grandma's house. \n\nWhen Red enters Grandma's house, she is surprised to see the young man sitting on the sofa.  \n\nGrandma smiles when she sees Red.", true, 4);
	createLink(4, 230, 265, 346);
		createRule(346, "link", "not", "and", true, 430);
			createCondition(nodeVisited, 6, 430, true, 433);
			createAction("enteredNode", 430, replaceText, [346, ""], 431);
	createLink(4, 135, 229, 343);
		createRule(343, "link", "not", "and", true, 435);
			createCondition(nodeVisited, 6, 435, false, 438);
			createAction("enteredNode", 435, replaceText, [343, ""], 436);
		createRule(4, "node", "if", "and", true, 480);
			createCondition(nodeVisited, 9, 480, true, 484);
			createCondition(nodeVisited, 4, 480, true, 483);
			createAction("anywhereCheck", 480, addAnywhereLink, [4], 481);
			createAction("enteredNode", 480, setFact, [378, false], 482);

	createNode("Go deeper into the forest", "A handsome young man is leaning against the trunk of a tree. He gestures to Red to come over.", true, 5);
		createRule(5, "node", "if", "and", true, 473);
			createCondition(nodeVisited, 9, 473, true, 479);
			createCondition(nodeVisited, 4, 473, true, 478);
			createCondition(nodeVisited, 5, 473, true, 477);
			createCondition(nodeVisited, 3, 473, false, 476);
			createAction("anywhereCheck", 473, addAnywhereLink, [5], 474);
			createAction("enteredNode", 473, setFact, [378, false], 475);

	createNode("Talk to young man", "Red goes over and talks to the wolf. He asks her where she's going, and she says she's off to deliver a basket of food and flowers to her sick grandma.", true, 6);
		createRule(6, "node", "if", "and", true, 467);
			createCondition(nodeVisited, 4, 467, true, 472);
			createCondition(nodeVisited, 9, 467, true, 471);
			createCondition(nodeVisited, 6, 467, true, 470);
			createCondition(nodeVisited, 5, 467, false, 469);
			createAction("anywhereCheck", 467, addAnywhereLink, [6], 468);

	createNode("Approach the young man", "Unfortunately, the young man was a wolf. Neither Red nor Grandma were ever seen again.", true, 7);
		createRule(7, "node", "if", "and", true, 461);
			createCondition(nodeVisited, 4, 461, false, 466);
			createCondition(nodeVisited, 6, 461, false, 465);
			createCondition(nodeVisited, 9, 461, true, 464);
			createCondition(nodeVisited, 7, 461, true, 463);
			createAction("anywhereCheck", 461, addAnywhereLink, [7], 462);

	createNode("Pass the basket to Grandma", "Red passes the basket of food and flowers to Grandma.", true, 8);
	createLink(8, 34, 41, 421);
		createRule(421, "link", "not", "and", true, 425);
			createCondition(checkBoolFact, 374, 425, true, 428);
			createAction("enteredNode", 425, replaceText, [421, 375], 426);
		createRule(8, "node", "if", "and", true, 455);
			createCondition(nodeVisited, 8, 455, true, 460);
			createCondition(nodeVisited, 9, 455, true, 459);
			createCondition(nodeVisited, 6, 455, true, 458);
			createCondition(nodeVisited, 4, 455, false, 457);
			createAction("anywhereCheck", 455, addAnywhereLink, [8], 456);

	createNode("Head home", "Red heads back home.", true, 9);
		createRule(9, "node", "if", "and", true, 451);
			createCondition(nodeVisited, 7, 451, true, 454);
			createAction("anywhereCheck", 451, addAnywhereLink, [9], 452);
			createAction("enteredNode", 451, setFact, [378, false], 453);

	createNode("Pick the geraniums", "Red decides to pick some of the geraniums in the grove and exchange them for the flowers in the basket for Grandma.", true, 376);
		createRule(376, "node", "if", "and", true, 445);
			createCondition(checkBoolFact, 374, 445, true, 450);
			createCondition(checkBoolFact, 378, 445, false, 449);
			createAction("anywhereCheck", 445, addAnywhereLink, [376], 446);
			createAction("enteredNode", 445, setFact, [375, "geraniums"], 447);
			createAction("enteredNode", 445, setFact, [374, true], 448);

	createNode("Pick the violets", "Red decides to pick some of the violets in the grove and exchange them for the flowers in the basket for Grandma.", true, 377);
		createRule(377, "node", "if", "and", true, 439);
			createCondition(checkBoolFact, 374, 439, true, 444);
			createCondition(checkBoolFact, 378, 439, false, 443);
			createAction("anywhereCheck", 439, addAnywhereLink, [377], 440);
			createAction("enteredNode", 439, setFact, [375, "violets"], 441);
			createAction("enteredNode", 439, setFact, [374, true], 442);

	createFact("Picked flowers", "boolean", 374);
	createFact("The flowers", "string", 375);
	createFact("In the forest grove", "boolean", 378);
}