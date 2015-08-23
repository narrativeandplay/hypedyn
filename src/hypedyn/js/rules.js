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
	
	// if no action in the rule consider it relevant as well
	// so that we can check fall through condition
	//if (rule.actions.length == 0)
	//	result = true;
		
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
		//debug
		if (func == setNumberFact) {
			disp("[set num fact] fired");
		}
	
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
 function eventTrigger( eventType, obj ) {
	disp("[event trigger] "+eventType );
    // we don't want to fire irrelevant rules
	// but this does throw away blocking information
	//var firing_candidates = filter_for_relevant( obj.rules, eventType );
	
	for ( var i=0; i<obj.rules.length; i++ ) {
		var rule = obj.rules[i];
		if ( checkCondition( rule ) ) {
		
			// fire all the actions
			for (var j in rule.actions) {
			
				// fire independent of whether it is relevant
				// the actions already takes care of that 	
				var action = rule.actions[j];
				action.doaction(eventType);
			}
			if ( ! rule.fall_through )
				break;
		}
	}
}

// simulate firing. 
// do fall through checks to see if a rule relev
function fall_through_till_relevant( eventType, rules ) {

	for ( var i=0; i< rules.length; i++ ) {
		var rule =  rules[i];
		if ( checkCondition( rule ) ) {
			// changed removed do action block
			
			// found a relevant rule so return it;
			if ( ruleRelevant( eventType, rule ) ) {
				return rule;
			}
			
			// met with blocking rule but not relevant
			if ( ! rule.fall_through ) {
				return false;
			}
		}
	}
	
	// fell through every rule, none of them has condition satisfied
	// OR if some were satisfied, none of those satisfied blocked and none are relevant
	return false;
}

 // if ready is true, we check whether the condition for 
 // that action is satisfied
 function link_clickable (link, ready) {
 
	//var fireable = firingCandidate( link.rules, "clickedLink", ready );
	
	// fireable = filter_out_empty_rules( fireable );
	
	//console.log("FIREABLE len "+fireable.length);
	//return ( fireable.length != 0 );
	
	// if ready, we're looking for clickable links
	if ( ready ) {
	    // find a firable rule with condition satisfied
		var fall_through_check = fall_through_till_relevant( "clickedLink", link.rules );
		
		disp( "fall through check "+fall_through_check );
		// clickable rule found
		if ( fall_through_check )
			return true;
		else                      
			return false;
			
	// if not ready, we're looking for the existence of actions making this link clickable 
	// (link can be possibly dormant links or already active)
	
	} else {
		disp("CHECKING FOR DORMANT");
		var relevant_rules = filter_for_relevant( link.rules, "clickedLink" );
		
		if (relevant_rules.length > 0)
			return true;
		else
			return false;
	}
}
 
 // NOT USED ANYMORE
// goes through the list of rules and filters out those not satisfied
/*
function filter_out_unsatisfied( rules ) {
	var to_return = [];
	for ( var i=0; i<rules.length; i++ ) {
		if ( checkCondition( rules[i] ) ) {
			to_return.push( rules[i] );
			if (! rules[i].fall_through)
				break;
		}
	}
	return to_return;
}
*/

function filter_for_relevant( rules, eventType ) {
	
	var to_return = [];
	for ( var i=0; i< rules.length; i++ ) {
		//disp('filter_for_relevant '+ rules[i].id);
		//disp('relevant? '+ruleRelevant( eventType, rules[i] ));
		if ( ruleRelevant( eventType, rules[i] ) ) {
			to_return.push( rules[i] );
		}
	}
	return to_return;
}

// NOT USED ANYMORE
// rules without actions
/*
function filter_out_empty_rules( rules ) {
	var to_return = []
	for ( var i=0; i< rules.length; i++ ) {
		if ( rules[i].actions.length > 0 ) {
			to_return.push( rules[i] );
		}
	}
	return to_return;
}
*/
	
 // NOT USED ANYMORE
 // determine the rules which are candidates for firing
 // rules: array of rules
 // eventType: ["clicked-links" "entered-node"]
 // checkCond: false just returns rules relevant to eventType, true means check the condition as well
 /*
 function firingCandidate( rules, eventType, checkCond ) {
	
	//var relevant_rules = filter_for_relevant( rules, eventType );
	
	if (checkCond) { // ready for firing
        // if need to check conditions then filter out unsatisfied
		var satisfied_rules = filter_out_unsatisfied( relevant_rules );
		return satisfied_rules;
	} else { // dormant
        // otherwise just filter for event type
		return relevant_rules;
	}
 }
 */

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
		alert("node undefined: " + nodeID);
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
 
 function compareNumFact( factID, comparator, operand_type, operand ) {
	//disp("comparator "+comparator);
	//disp("operand_type "+operand_type);
	//disp("type of operand "+operand.constructor);
	//disp("operand "+operand);
	var right_operand = false;
	switch ( operand_type ) {
		case "Input":
			right_operand = operand;
			break;
		case "Fact":
			right_operand = getFact( operand ).value.toString();
			break;
	}
	
	if (comparator === "=")
		comparator = "==";
		
	var left_operand = getFact( factID ).value.toString();
	 disp(" evaluating "+left_operand + comparator + right_operand);
	 var result = eval( left_operand + comparator + right_operand )
	 disp("compareNumFact result "+result);
	return result;
 }
 
 function nodeIsPrevious(nodeID) {
    //disp("*** nodeIsPrevious! ***");
    //disp("prev_read_nodes len: " + prev_read_nodes.length)
    //disp("nodeID: "+nodeID+", prev_read_node:"+prev_read_nodes[prev_read_nodes.length - 1]);
    //disp("");
	return prev_read_nodes[prev_read_nodes.length - 1] == nodeID;
 }
 
 function createCondition(func, func_args_arr, ruleID, not, id) {
	//disp("*** not: "+not);
    
    not = (not == undefined) ? false : not;
	id = (id == undefined) ? genID(conditionlist) : id;
	
	var newCond = new Object();
	newCond.eval = function () {
		//disp(func.toString());
		if (not) {
			return (! func.apply(this, func_args_arr) );
		}
		else {
			return func.apply(this, func_args_arr);
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
	
	disp("Check condition "+rule.id);
	disp(" result "+result);
	
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
 
