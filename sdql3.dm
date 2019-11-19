
#define TOK(a,b) tokens[++tokens.len] = list(a,b)
#define TOK_IS(t,a,b)  ((t[1]) == (a) && (t[2]) == (b))

#define TOK_SELECT   "SELECT"
#define TOK_COUNT    "COUNT"
#define TOK_DISTINCT "DISTINCT"
#define TOK_AND      "AND"
#define TOK_OR		 "OR"
#define TOK_UPDATE   "UPDATE"
#define TOK_SET      "SET"
#define TOK_FROM     "FROM"
#define TOK_WHERE    "WHERE"
#define TOK_DELETE   "DELETE"
#define TOK_CALL     "CALL"
#define TOK_ON       "ON"
#define TOK_NOT      "NOT"

#define TOK_EQUAL    "=="
#define TOK_ASSIGN   "="
#define TOK_LEQUAL   "<="
#define TOK_NEQUAL   "!="
#define TOK_GEQUAL   ">="
#define TOK_LESS     "<"
#define TOK_GREATER  ">"
#define TOK_OPENBRKT "("
#define TOK_CLOSBRKT ")"
#define TOK_COMMA    ","
#define TOK_PLUS     "+"
#define TOK_MINUS    "-"
#define TOK_DIVIDE   "/ " // space to avoid conflicting with /type
#define TOK_STAR     "*"
#define TOK_BITXOR   "^"
#define TOK_BITAND   "&"
#define TOK_BITOR    "|"

#define TOK_BITNOT   "~"

/*
	SDQL3

	[ ] - zero or one of the following
	{ } - zero or more of the following
	< > - grouping
	/.../ - regex

	Query formats:

		SELECT values [ WHERE cond ]
		SELECT < * | COUNT(*) | [ DISTINCT ] values > FROM /type [ WHERE cond ]

		UPDATE /type SET set_values [ WHERE cond ]

		CALL function([ values ]) [ ON value ] [ WHERE cond ]

		DELETE /type [ WHERE cond ]

	Other stuff:

		values: value { , value }
		set_values: var = value { , var = value }

		value: src | var | literal

		var: global . path | src . path | path

		path: ident [ . path ]

		ident: /[a-z]+/

*/
/proc/sdql3(q)
	var/datum/sdql3/S = new(q)
	var/r = S.tokenise()
	if(istext(r))
		world.log << "tokenisation error: [r]"
		return

	r = S.parse()
	if(istext(r))
		world.log << "parse error: [r]"
		return

	return S.execute()

/var/list/sdql3_whitespace = list(" ", "\t", "\n")
/var/list/sdql3_token_list = list(
	TOK_SELECT,
	TOK_COUNT,
	TOK_DISTINCT,
	TOK_AND,
	"&&" = TOK_AND,
	"||" = TOK_OR,
	TOK_OR,
	TOK_UPDATE,
	TOK_SET,
	TOK_FROM,
	TOK_WHERE,
	TOK_DELETE,
	TOK_CALL,
	TOK_ON,
	TOK_EQUAL,
	TOK_ASSIGN,
	TOK_LEQUAL,
	TOK_NEQUAL,
	"<>" = TOK_NEQUAL,
	TOK_GEQUAL,
	TOK_LESS,
	TOK_GREATER,
	TOK_OPENBRKT,
	TOK_CLOSBRKT,
	TOK_COMMA,
	TOK_PLUS,
	TOK_MINUS,
	TOK_DIVIDE,
	TOK_STAR,
	TOK_BITXOR,
	TOK_BITAND,
	TOK_BITOR,
	TOK_BITNOT,
	TOK_NOT,
	"!" = TOK_NOT,
)

/datum/sdql3
	var/q
	var/list/tokens
	var/list/tree

/datum/sdql3/New(query)
		..()
		q = query

/datum/sdql3/proc/tokenise()
	var/text = q
	var/i = 1
	tokens = list()

	tok:
		while(i <= length(text))
			while(copytext(text, i, i+1) in sdql3_whitespace)
				i++

			if(i > length(text))
				break

			for(var/t in sdql3_token_list)
				if(cmptext(t, copytext(text, i, i+length(t))))
					i += length(t)
					if(sdql3_token_list[t])
						t = sdql3_token_list[t] // "<>" => "!="
					TOK("kw", t)
					continue tok

			if(text2ascii(text,i) in list(34, 39))
				var/start = i
				var/sc = text2ascii(text, i)
				i++
				while(i <= length(text) && text2ascii(text, i) != sc)
					i++
				if(text2ascii(text, i) != sc)
					return "reached end of query inside a string"

				TOK("id", list("lit", copytext(text, start+1, i)))
				i++
				continue tok

			var/start = i
			id:
				while(1)
					switch(text2ascii(text, i))
						if(65 to 90, 97 to 122, 48 to 57, 46, 47) // A-Z, a-z, 0-9, ., /
							i++
						else
							break id

			if(start == i)
				return "unexpected character at index [i]: [copytext(text, i, i+1)]"

			var/ident = copytext(text, start, i)

			TOK("id", parse_ident(ident))

	return 0

/datum/sdql3/proc/parse_ident(ident)
	if(isnum(text2num(ident)))
		return list("lit", text2num(ident))
	else if(ident == "null")
		return list("lit", null)
	else if(ident == "false")
		return list("lit", 0)
	else if(ident == "true")
		return list("lit", 1)
	else if(copytext(ident, 1, 2) == "/" || ident == "world")
		return list("typ", ident)
	else
		return list("var", splittext(ident, "."))


/datum/sdql3/proc/parse()
	var/grouping_kw = list(TOK_SELECT, TOK_UPDATE, TOK_SET, TOK_FROM, TOK_WHERE, TOK_DELETE, TOK_CALL, TOK_ON)

	var/list/groups = list()
	var/list/grp = list()
	for(var/i = length(tokens); i > 0; i--)
		grp.Insert(1, list(tokens[i]))
		if((tokens[i][1] == "kw") && (tokens[i][2] in grouping_kw))
			groups.Insert(1, list(grp))
			grp = list()

	tree = list()

	for(grp in groups)
		var/type = grp[1][2]
		if(type in tree)
			return "unexpected repeat of keyword [type] at [render_group(grp)]"

		var/ret
		switch(type)
			if(TOK_SELECT)
				ret = parse_select(grp)
			if(TOK_UPDATE, TOK_FROM, TOK_DELETE, TOK_ON)
				ret = parse_X_type(grp)
			if(TOK_CALL)
				ret = parse_call(grp)
			if(TOK_WHERE)
				ret = parse_where(grp)
			if(TOK_SET)
				ret = parse_set(grp)
			else
				return "unexpected group type [type] - code bug!"

		if(istype(ret, /list))
			tree[type] = ret
		else
			return ret

	return 0

/datum/sdql3/proc/render_group(list/grp)
	. = list()

	for(var/tok in grp)
		. += tok[2]

	return jointext(., " ")

/datum/sdql3/proc/parse_select(list/grp)
	// Expected: "SELECT" ident ("," ident)*
	// Or:       "SELECT" "DISTINCT" ident ("," ident)*
	// Or:       "SELECT" "COUNT" "(" "*" ")"
	// Or:       "SELECT" "*"

	if(length(grp) == 2 && TOK_IS(grp[2], "kw", TOK_STAR))
		return list(TOK_STAR = TRUE)

	if(length(grp) == 5 && TOK_IS(grp[2], "kw", TOK_COUNT) && TOK_IS(grp[3], "kw", TOK_OPENBRKT) && TOK_IS(grp[4], "kw", TOK_STAR) && TOK_IS(grp[5], "kw", TOK_CLOSBRKT))
		return list(TOK_STAR = TRUE, TOK_COUNT = TRUE)

	var/list/select = list()

	if(TOK_IS(grp[2], "kw", TOK_DISTINCT))
		select[TOK_DISTINCT] = TRUE
		grp.Cut(2,3)

	var/list/select_idents = list()
	var/i = 2
	while(i <= length(grp))
		var/comma = h_find_first_tok(grp, start=i, type=TOK_COMMA)
		if(comma == 0)
			comma = length(grp) + 1

		var/list/expr = parse_value(grp.Copy(i, comma))
		if(istext(expr))
			return expr
		select_idents[++select_idents.len] = expr
		i = comma + 1

	select["IDENTS"] = select_idents
	return select


/datum/sdql3/proc/parse_X_type(list/grp)
	// Expected: X type
	if(length(grp) != 2)
		return "unexpected token count at [render_group(grp)]"
	if(grp[2][1] != "id")
		return "unexpected token type at [render_group(grp)]"
	if(grp[2][2][1] != "typ")
		return "unexpected ident type at [render_group(grp)]"

	return list(grp[2][2])

/datum/sdql3/proc/parse_call(list/grp)
	// Expected: "CALL" function "(" value ("," value)* ")"

	var/list/c = list()

	if(!TOK_IS(grp[3], "kw", TOK_OPENBRKT))
		return "unexpected CALL format [render_group(grp)]"
	if(!TOK_IS(grp[length(grp)], "kw", TOK_CLOSBRKT))
		return "unexpected CALL format [render_group(grp)]"
	if(grp[2][1] != "id")
		return "unexpected token type at [render_group(grp)]"

	c["FUNCTION"] = grp[2][2][2]

	var/list/arguments = list()
	var/i = 4
	while(i < length(grp))
		var/comma = h_find_first_tok(grp, start=i, type=TOK_COMMA)
		if(comma == 0)
			comma = length(grp) // skip closing )

		var/list/expr = parse_value(grp.Copy(i, comma))
		if(istext(expr))
			return expr
		arguments[++arguments.len] = expr
		i = comma + 1
	c["ARGUMENTS"] = arguments

	return c

/datum/sdql3/proc/parse_where(list/grp)
	// Expected: "WHERE" boolean-condition
	return parse_value(grp.Copy(2))

/datum/sdql3/proc/parse_set(list/grp)
	// Expected: "SET" ident "=" value ("," ident "=" value)*

	var/list/sets = list()

	var/i = 1
	while(i <= length(grp))
		if(i+3 > length(grp))
			return "invalid SET expression at [render_group(grp.Copy(i+1))]"

		if(!TOK_IS(grp[i+2], "kw", TOK_ASSIGN))
			return "invalid SET expression at [render_group(grp.Copy(i+1))]"

		if(grp[i+1][1] != "id")
			return "left operand of = must be an identifier at [render_group(grp.Copy(i+1))]"

		var/ident = grp[i+1][2]
		if(ident in sets)
			return "duplicated SET variable [ident] at [render_group(grp.Copy(i+1))]"

		var/comma = h_find_first_tok(grp, start=i+2, type=TOK_COMMA)
		if(comma == 0)
			comma = length(grp)+1

		var/list/expr = parse_value(grp.Copy(i+3, comma))
		if(istext(expr))
			return expr
		sets[ident] = expr
		i = comma

	return sets


/var/list/sdql3_precedence = list(
	list(TOK_BITNOT, TOK_NOT, TOK_MINUS) = TRUE,
	list(TOK_STAR, TOK_DIVIDE) = FALSE,
	list(TOK_PLUS, TOK_MINUS) = FALSE,
	list(TOK_BITXOR, TOK_BITOR, TOK_BITAND) = FALSE,
	list(TOK_EQUAL, TOK_LEQUAL, TOK_NEQUAL, TOK_GEQUAL, TOK_LESS, TOK_GREATER) = FALSE,
	list(TOK_AND, TOK_OR) = FALSE,
)

/datum/sdql3/proc/parse_value(list/expr)
	bexpr = expr
	bi = 1
	return brackets()

/datum/sdql3
	var/list/bexpr
	var/bi
/datum/sdql3/proc/brackets()
	var/list/ret = list()
	while(bi <= length(bexpr))
		if(TOK_IS(bexpr[bi], "kw", TOK_OPENBRKT))
			bi++
			ret[++ret.len] = brackets()
		else if(TOK_IS(bexpr[bi], "kw", TOK_CLOSBRKT))
			bi++
			return list("expr", reduce_expression(ret))
		else
			ret[++ret.len] = bexpr[bi]
			bi++
	return list("expr", reduce_expression(ret))

/datum/sdql3/proc/reduce_expression(list/expr)
	group:
		while(length(expr) != 1)
			for(var/opgrp in sdql3_precedence)
				var/unary = sdql3_precedence[opgrp]
				for(var/op in opgrp)
					var/i = h_find_first_tok(expr, type=op)
					if(i != 0)
						if(!unary && i == 1)
							return "unexpected operator [op] at beginning of expression [render_expr(expr)]"
						if(i == length(expr))
							return "unexpected operator [op] at end of expression [render_expr(expr)]"

						if(unary && op == TOK_MINUS && i != 1) // special-case for -
							var/left = expr[i-1]
							if(left[1] != "kw")
								continue

						if(unary)
							var/right = expr[i+1]
							expr.Cut(i+1, i+2)
							expr[i] = list(op, right)
						else
							var/left = expr[i-1]
							var/right = expr[i+1]
							expr.Cut(i, i+2)
							expr[i-1] = list(op, left, right)
						continue group

			return "invalid value expression at [render_expr(expr)]"

	return expr[1]

/datum/sdql3/proc/render_expr(e)
	return json_encode(e)

/datum/sdql3/proc/h_find_first_tok(list/toks, start=1, type=TOK_COMMA)
	for(var/i = start; i < length(toks); i++)
		if(TOK_IS(toks[i], "kw", type))
			return i
	return 0

/datum/sdql3/proc/set_var(list/ident, value, datum/D)
	switch(ident[1])
		if("global")
			return set_global(ident.Copy(2), value, D)
		if("src")
			return set_var(ident.Copy(2), value, D)
		else
			if(D && (ident[1] in D.vars))
				return set_datumvar(ident, value, D)
			if(ident[1] in global.vars)
				return set_global(ident, value)
			return "attempt to set unknown variable [ident[1]]"

/datum/sdql3/proc/set_global(list/vname, value)
	if(length(vname) == 1)
		global.vars[vname[1]] = value
	else
		return set_datumvar(vname.Copy(2), value, global.vars[vname[1]])

/datum/sdql3/proc/set_datumvar(list/vname, value, datum/D)
	if(length(vname) == 1)
		D.vars[vname[1]] = value
	else
		set_datumvar(vname.Copy(2), value, D.vars[vname[1]])

/datum/sdql3/proc/get_ident(list/ident, datum/D)
	switch(ident[1])
		if("lit") return ident[2]
		if("typ") return text2path(ident[2])
		if("var")
			switch(ident[2][1])
				if("src")
					if(length(ident[2]) == 1)
						return D
					else
						return get_var(ident[2].Copy(2), D)
				if("global")
					return get_global(ident[2].Copy(2))
				if("world")
					return get_var(ident[2].Copy(2), world)
				else
					if(D && (ident[2][1] in D.vars))
						return get_var(ident[2], D)
					if(ident[2][1] in global.vars)
						return get_global(ident[2])
					throw "attempt to get unknown variable [ident[2][1]]"
		else throw "invalid ident type [ident[1]]"

/datum/sdql3/proc/get_global(list/vname)
	if(length(vname) == 1)
		return global.vars[vname[1]]
	else
		return get_var(vname.Copy(2), global.vars[vname[1]])

/datum/sdql3/proc/get_var(list/vname, datum/D)
	if(length(vname) == 1)
		return D.vars[vname[1]]
	else
		return get_var(vname.Copy(2), D.vars[vname[1]])

/datum/sdql3/proc/eval_expr(list/tree, datum/D)
	switch(tree[1])
		if("id")
			return get_ident(tree[2], D)
		if("expr")
			return eval_expr(tree[2], D)

	var/a = eval_expr(tree[2], D)
	var/b = null
	if(length(tree) > 2)
		b = eval_expr(tree[3], D)
	switch(tree[1])
		if(TOK_EQUAL, TOK_ASSIGN) return a == b
		if(TOK_NEQUAL) return a != b
		if(TOK_GEQUAL) return a >= b
		if(TOK_LEQUAL) return a <= b
		if(TOK_LESS) return a < b
		if(TOK_GREATER) return a > b
		if(TOK_DIVIDE) return a / b
		if(TOK_STAR) return a * b
		if(TOK_PLUS) return a + b
		if(TOK_MINUS)
			if(length(tree) == 2) // unary -
				return -a
			else // binary -
				return a - b
		if(TOK_AND) return a && b
		if(TOK_OR) return a || b
		if(TOK_BITOR) return a | b
		if(TOK_BITAND) return a & b
		if(TOK_BITXOR) return a ^ b
		if(TOK_NOT) return !a
		if(TOK_BITNOT) return ~a
		else
			throw "invalid operator [tree[1]]"

/datum/sdql3/proc/execute()
	try
		if(TOK_SELECT in tree)
			var/select = tree[TOK_SELECT]

			var/list/things = list(null)
			if(TOK_FROM in tree)
				if(tree[TOK_FROM][1][1] != "typ")
					return "invalid FROM argument [tree[TOK_FROM][1][2]]"

				var/type = tree[TOK_FROM][1][2]

				things = get_from(type)

			if(TOK_WHERE in tree)
				var/where = tree[TOK_WHERE]
				for(var/thing in things)
					if(!eval_expr(where, thing))
						things -= thing

			if(select[TOK_STAR] && select[TOK_COUNT])
				// SELECT COUNT(*)
				return length(things)

			if(select[TOK_STAR])
				// SELECT *
				return things

			else if(select[TOK_DISTINCT])
				// SELECT DISTINCT ident, ident, ident
				var/list/values = list()

				var/list/idents = select["IDENTS"]
				var/lastident = idents[length(idents)]
				idents = idents.Copy(1, length(idents))

				for(var/thing in things)
					var/v = values
					for(var/ident in idents)
						var/value = eval_expr(ident, thing)
						var/key = v2k(value)
						if(!(key in v))
							v[key] = list()

						v = v[key]

					var/value = eval_expr(lastident, thing)
					var/key = v2k(value)
					v[key] = TRUE

				return collapse_nested(values)

			else
				// SELECT ident, ident, ident
				var/list/values = list()
				var/list/idents = select["IDENTS"]

				for(var/thing in things)
					var/list/v = list()
					for(var/ident in idents)
						var/val = eval_expr(ident, thing)
						v[++v.len] = val
					values[++values.len] = v

				return values

		else if((TOK_UPDATE in tree) && (TOK_SET in tree))
			
			var/update = tree[TOK_SET]
			var/type = tree[TOK_UPDATE][1][2]

			var/list/things = get_from(type)
			if(TOK_WHERE in tree)
				var/where = tree[TOK_WHERE]
				for(var/thing in things)
					if(!eval_expr(where, thing))
						things -= thing

			for(var/thing in things)
				for(var/ident in update)
					if(ident[1] != "var")
						return "invalid ident type for SET: [ident[1]]"

					var/value = eval_expr(update[ident], thing)
					set_var(ident[2], value, thing)

			return length(things)

		else if(TOK_DELETE in tree)
			var/type = tree[TOK_DELETE][1][2]

			var/list/things = get_from(type)
			if(TOK_WHERE in tree)
				var/where = tree[TOK_WHERE]	
				for(var/thing in things)
					if(!eval_expr(where, thing))
						things -= thing

			for(var/thing in things)
				del thing // TODO: qdel

			return length(things)

		else if(TOK_CALL in tree)
			var/c = tree[TOK_CALL]
			var/list/things = list(null)
			if(TOK_ON in tree)
				var/type = tree[TOK_ON][1][2]

				things = get_from(type)

			if(TOK_WHERE in tree)
				var/where = tree[TOK_WHERE]
				for(var/thing in things)
					if(!eval_expr(where, thing))
						things -= thing

			var/list/ar = c["ARGUMENTS"]
			var/list/fn = c["FUNCTION"]

			var/function = fn[length(fn)]
			var/is_global = 0
			var/path = list()
			if(length(fn) > 1)
				path = fn.Copy(1, length(fn))
				if(path[1] == "global" && length(path) == 1)
					function = "/proc/[function]"
					is_global = 1
					if(isnull(text2path(function)))
						return "function [function] does not exist!"

			for(var/thing in things)
				var/list/a = list()
				for(var/argument in ar)
					a[++a.len] = eval_expr(argument, thing)

				if(is_global)
					call(function)(arglist(a))
				else
					if(length(path))
						thing = get_ident(list("var",path), thing)

					var/datum/t = thing
					if(!hascall(thing, function))
						return "function [t.type]/proc/[function] does not exist!"

					call(thing, function)(arglist(a))
			
			return length(things)

		else
			return "unknown expression [json_encode(tree)]"

	catch(var/exception/e)
		if(istext(e))
			return e
		else
			world.log << "caught [e] [e.file] [e.line]"


/datum/sdql3/proc/collapse_nested(list/n)
	if(n[n[1]] == TRUE) // last level
		var/list/l = list()
		for(var/key in n)
			l[++l.len] = list(k2v(key))
		return l

	var/list/l = list()
	for(var/key in n)
		for(var/vv in collapse_nested(n[key]))
			var/list/lvv = vv
			l[++l.len] = list(k2v(key)) + lvv.Copy()
	return l

/datum/sdql3/proc/get_from(f)
	if(f == "world")
		return list(world)

	var/type = text2path(f)
	if(!type)
		throw "invalid FROM argument [f]"

	. = list()
	if(ispath(type, /mob))
		for(var/mob/M in world)
			if(istype(M, type))
				. += M

	else if(ispath(type, /obj))
		for(var/obj/O in world)
			if(istype(O, type))
				. += O

	else if(ispath(type, /area))
		for(var/area/A in world)
			if(istype(A, type))
				. += A

	else if(ispath(type, /turf))
		for(var/turf/T in world)
			if(istype(T, type))
				. += T

	else if(type == /client)
		for(var/client/C)
			. += C

	else if(ispath(type, /atom/movable))
		for(var/atom/movable/AM in world)
			if(istype(AM, type))
				. += AM


	else
		for(var/datum/D)
			if(istype(D, type))
				. += D

/proc/v2k(v)
	if(isnum(v))
		return "[num_pre][v]"
	return v

var/num_pre = "__SDQL_NUM__"
/proc/k2v(k)
	if(istext(k) && copytext(k, 1, length(num_pre)+1) == num_pre)
		return text2num(copytext(k, length(num_pre)+1))
	return k
