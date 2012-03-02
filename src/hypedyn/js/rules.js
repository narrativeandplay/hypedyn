// links can have follow link (to node), change of text and update facts
// nodes can have update facts
// NOTE: special case, links in anywhere nodes cannot have follow link action

// datastructure list
var rulelist = [];
var actionlist = [];
var conditionlist = [];

/*
 * Rules
 */
 // and_or decides whether all the conditions are joined by ANDs or ORs ["and", "or"]
 // if_unless decides whether all the conditions are negated, ["if", "not"]
function createRule(parentID, parentType, if_not, and_or, fall_through, id) {
	// init
	var newrule = new Object();
	newrule.conditions = [];
	newrule.actions = [];
	
	// handling default value if argument not passed in
	id = (typeof id == "undefined") ? genID(nodelist) : id;
	if_not = (typeof if_not == "undefined") ? "if" : if_not;
	and_or = (typeof and_or == "undefined") ? "or" : and_or;
	fall_through = (typeof fall_through == "undefined") ? true : fall_through;
	
	newrule.id = id;
	newrule.fall_through = fall_through;
	newrule.if_not = if_not;
	newrule.and_or = and_or;
	
	newrule.parentID = parentID;
	newrule.fall_through = fall_through;
	switch (parentType) {
		case "link":
			var parent = linklist[parentID];
			parent.rules[parent.rules.length] = newrule;
			break;
		case "node":
			//var parent = nodelist.get(parentID);
			parent = nodelist[parentID];
			parent.rules[parent.rules.length] = newrule;
			break;
	}
	
	// Methods
	newrule.addAction = function (newaction) {
		newrule.actions[newrule.actions.length] = newaction;
	}
	newrule.addCondition = function (newcond) {
		newrule.conditions[newrule.conditions.length] = newcond;
	}
	
	rulelist[id] = newrule;
	return newrule;
}

// go through all the actions and see whether the eventType 
// triggers any of the actions in this rule
function ruleRelevant(evenType, rule) {
	
}

/*
 *	Actions
 */ 
 
 function createAction(eventType, parentRuleID, func, args, id) {
	var newaction = new Object();
	newaction.eventType = eventType;
	
	newaction.id = id;
	newaction.args = args; // array of to be passed to the function
	newaction.doaction = function (evtType) {
		
		if (func == undefined)
			func = function () {}
		if (args == undefined)
			args = [];
		
		if (newaction.eventType == evtType) {
			func.apply(this, args);
			return true; // indicate action fired
		} else {
			return false;
		}
	}
	
	if (rulelist[parentRuleID] != undefined) {
		rulelist[parentRuleID].addAction(newaction);
	}
	
	return newaction;
 }
 
 // actions are bound to the event types that trigger them. 
 // change of text is triggered only when entering a node
 // update facts
 //   links: triggered by clicking links
 //   nodes: triggered by entering nodes
 // follow link triggered only when clicking link (only available on links as well)
 
 // eventType can be one of these ["clicked-links" "entered-node"]
 // goes through all the rules in this obj
 function eventTrigger(eventType, obj) {
	for (var i in obj.rules) {
		var rule = obj.rules[i];
		if ( checkCondition(rule) ) {
			var fired = false;
			for (var j in rule.actions) {
				var action = rule.actions[j];
				fired = action.doaction(eventType) || fired;
			}

			// stop evaluating if it is not suppose to fall_through
			if ((!rule.fall_through) && fired) {
				break;
			}
		}
	}
 }
 
/*
 *	Conditions
 */	
 
 function nodeVisited(nodeID) {
	//var node = nodelist.get(nodeID);
	node = nodelist[nodeID];
	if (node != undefined) {
		return node.visited;
	}
 }
 
 function linkFollowed(linkID) {
	var link = linklist[linkID];
	if (link != undefined) {
		return link.followed > 0;
	}
 }
 
 function checkBoolFact(factID) {
	var fact = factlist[factID];
	if (fact != undefined && fact.type == "boolean") {
		return fact.value;
	}
 }
 
 function createCondition(func, func_target_ID, ruleID, not, id) {
	not = (not == undefined) ? false : not;
	id = (id == undefined) ? genID(conditionlist) : id;
	
	var newCond = new Object();
	newCond.eval = function () {
		//console.log("eval in condition ");
		//console.log(func.toString());
		if (not) {
			//console.log ("in not ");
			//console.log((! func.apply(this, [func_target_ID]) ));
			return (! func.apply(this, [func_target_ID]) );
		}
		else {
			//console.log ("in if ");
			//console.log( func.apply(this, [func_target_ID]));
			return func.apply(this, [func_target_ID]);
		}
	}
	
	if (ruleID != undefined) {
		var rule = rulelist[ruleID];
		if (rule != undefined) {
			rule.addCondition(newCond);
		}
	}
	return newCond;
 }
 
 function checkCondition(rule) {
	//rule.if_not;
	var result;
	switch (rule.and_or) {
		case "and":
			// make sure all conditions evaluate to true
			for (var i in rule.conditions) {
				var cond = rule.conditions[i];
				if (! cond.eval()) {
					result = false;
					break;
				}
			}
			// if result wasnt set to false then all conditions are true
			result = (result == undefined) ? true : false;
			break;
		case "or":
			
			if (rule.conditions.length == 0)
				result = true;
				
			// only one condition true will do
			for (var i in rule.conditions) {
				var cond = rule.conditions[i];
				if (cond.eval()) {
					result = true;
					break;
				}
			}
			// if result wasnt set to false then all conditions are true
			result = (result == undefined) ? false : true;
			break;
	} // end of and_or switch
	
	// negation if not
	switch (rule.if_not) {
		case "if":
			return result;
			break;
		case "not":
			return !result;
			break;
	}
 }
 
 function findAction() {
	
 }
 
