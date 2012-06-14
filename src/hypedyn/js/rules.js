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
function ruleRelevant(eventType, rule) {
	var result = false;
	for (var i in rule.actions) {
		if (rule.actions[i].eventType == eventType) {
			//console.log("action tyep "+ rule.actions[i].eventType);
			//console.log(" in type "+ eventType);
			result = true;
			break;
		}
	}
	return result;
}

/*
 *	Actions
 */ 
 
 // eventType can be "enteredNode" "clickedLink" "anywhereCheck"
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
	} else {
		alert("parent rule we're adding action to is undefined!");
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
	var rules_to_fire = firingCandidate( obj, eventType, true);
	//disp("rules to fire "+rules_to_fire.length); 
	for (var i in rules_to_fire) {
		var rule = rules_to_fire[i];
		//var fired = false;
		for (var j in rule.actions) {
			var action = rule.actions[j];
			action.doaction(eventType); //|| fired;
		}
		// stop evaluating if it is not suppose to fall_through
		if ((!rule.fall_through) ) {//&& fired) {
			break;
		}
	}
 }

 // if ready is true, we check whether the condition for 
 // that action is satisfied
 function link_clickable (link, ready) {
	var fireable = firingCandidate( link, "clickedLink", ready);
	//console.log("FIREABLE len "+fireable.length);
	return fireable.length > 0;
}
 
 // determine the rules which are candidates for firing
 // obj: the object containing the rules (currently node or link)
 // eventType: ["clicked-links" "entered-node"]
 // checkCond: true means check the condition
 function firingCandidate( obj, eventType, checkCond ) {
 
    // goes through the list of rules and filters out those not satisfied
	function filter_out_unsatisfied( rules, index, arr ) {
		if ( index < rules.length ) {
			if (checkCondition(rules[index]))
				arr[arr.length] = rules[index];
			return filter_out_unsatisfied( rules, index+1, arr );
		} else {
			return arr;
		}
	}
	
    // filters for event type and fall-through
    function helper ( rules, index, arr ) {
		if ( index < rules.length ) {
            // check if rule is relevant to eventType
            if (ruleRelevant(eventType, rules[index])) {
					arr[arr.length] = rules[index];
            }
            
            // not checking conditions, or fallthrough is true, so continue
			if (! (checkCond && (! rules[index].fall_through) )) { 
				return helper ( rules, index+1, arr );
			} else {
                // otherwise stop now
                return arr;
            }
		} else {
            // finished
			return arr;
		}
	}
	
    // actual firingCandidate code starts here
	if (checkCond) {
        // if need to check conditions then filter out unsatisfied first
		var satisfied_rules = filter_out_unsatisfied( obj.rules, 0, [] );
        // then filter for event type and fall-through
		return helper ( satisfied_rules, 0, [] );
	} else {
        // otherwise just filter for event type and fall-through
		return helper ( obj.rules, 0, [] );
	}
 }
 

/*
 *	Conditions
 */	
 
 function nodeVisited(nodeID) {
	//if (nodeID == 78) {
	//	alert("node visited check ");
	//}
	//var node = nodelist.get(nodeID);
	node = nodelist[nodeID];
	if (node != undefined) {
		//if (nodeID == 78) {
		//	alert("node visited? "+node.visited);
		//}
		return node.visited;
	} else {
		alert("node undefined");
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
 
 function nodeIsPrevious(nodeID) {
    //disp("*** nodeIsPrevious! ***");
    //disp("nodeID: "+nodeID+", prev_read_node:"+prev_read_nodes[prev_read_nodes.length - 1]);
    //disp("");
	return prev_read_nodes[prev_read_nodes.length - 1] == nodeID;
 }
 
 function createCondition(func, func_target_ID, ruleID, not, id) {
	//disp("*** not: "+not);
    
    not = (not == undefined) ? false : not;
	id = (id == undefined) ? genID(conditionlist) : id;
	
	var newCond = new Object();
	newCond.eval = function () {
		//disp(func.toString());
		if (not) {
			//disp("in not ");
			//disp((! func.apply(this, [func_target_ID]) ));
			return (! func.apply(this, [func_target_ID]) );
		}
		else {
			//disp("in if ");
			//disp( func.apply(this, [func_target_ID]));
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
 
