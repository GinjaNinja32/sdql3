
#define TOK(a,b) tokens[++tokens.len] = list(a,b)
#define TOK_IS(t,a,b)  ((t[1]) == (a) && (t[2]) == (b))

#define TOK_EXPLAIN  "EXPLAIN"

#define TOK_SELECT   "SELECT"
#define TOK_COUNT    "COUNT"
#define TOK_DISTINCT "DISTINCT"

#define TOK_UPDATE   "UPDATE"
#define TOK_SET      "SET"

#define TOK_DELETE   "DELETE"

#define TOK_FROM     "FROM"
#define TOK_WHERE    "WHERE"

#define TOK_AND      "AND"
#define TOK_OR		 "OR"
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
#define TOK_NEW      "new"

#define TYP_KEYWORD    "keyword"
#define TYP_ARGLIST    "arglist"
#define TYP_CALL       "call"
#define TYP_EXPRESSION "expression"
#define TYP_IDENTIFIER "identifier"
#define TYP_NEW        "new"

#define IDTYP_LITERAL  "lit"
#define IDTYP_VARIABLE "var"

#define SELECT_COLUMNS "columns"
#define COUNT_CONDITION "condition"

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
	try
		var/datum/sdql3/S = new(q)
		var/r = S.tokenise()
		if(istext(r))
			return r

		r = S.parse()
		if(istext(r))
			return r

		return S.execute()
	catch(var/exception/e)
		if(istext(e))
			return e
		else
			return "caught [e] at [e.file] [e.line]"

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
	TOK_EQUAL,
	TOK_ASSIGN,
	TOK_EXPLAIN,
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
	TOK_NEW,
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
					TOK(TYP_KEYWORD, t)
					continue tok

			if(text2ascii(text,i) in list(34, 39))
				var/start = i
				var/sc = text2ascii(text, i)
				i++
				while(i <= length(text) && text2ascii(text, i) != sc)
					i++
				if(text2ascii(text, i) != sc)
					return "reached end of query inside a string"

				TOK(TYP_IDENTIFIER, list(IDTYP_LITERAL, copytext(text, start+1, i)))
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
				return "unexpected character at index [i]: '[copytext(text, i, i+1)]'"

			var/ident = copytext(text, start, i)

			TOK(TYP_IDENTIFIER, parse_ident(ident))

	return 0

/datum/sdql3/proc/parse_ident(ident)
	if(isnum(text2num(ident)))
		return list(IDTYP_LITERAL, text2num(ident))
	else if(ident == "null")
		return list(IDTYP_LITERAL, null)
	else if(ident == "false")
		return list(IDTYP_LITERAL, 0)
	else if(ident == "true")
		return list(IDTYP_LITERAL, 1)
	else if(ident == "world")
		return list(IDTYP_LITERAL, world)
	else if(copytext(ident, 1, 2) == "/")
		var/p = text2path(ident)
		if(!p)
			throw "nonexistent type '[ident]'"
		return list(IDTYP_LITERAL, p)
	else
		return list(IDTYP_VARIABLE, splittext(ident, "."))

/datum/sdql3/proc/parse()
	var/grouping_kw = list(TOK_EXPLAIN, TOK_SELECT, TOK_UPDATE, TOK_SET, TOK_FROM, TOK_WHERE, TOK_DELETE)

	var/list/groups = list()
	var/list/grp = list()
	for(var/i = length(tokens); i > 0; i--)
		grp.Insert(1, list(tokens[i]))
		if((tokens[i][1] == TYP_KEYWORD) && (tokens[i][2] in grouping_kw))
			groups.Insert(1, list(grp))
			grp = list()

	tree = list()

	for(grp in groups)
		var/type = grp[1][2]
		if(type in tree)
			throw "unexpected repeat of keyword '[type]' at '[render_group(grp)]'"

		var/ret
		switch(type)
			if(TOK_SELECT)
				ret = parse_select(grp)
			if(TOK_UPDATE, TOK_FROM, TOK_DELETE)
				ret = parse_x_type(type, grp)
			if(TOK_WHERE)
				ret = parse_where(grp)
			if(TOK_SET)
				ret = parse_set(grp)
			if(TOK_EXPLAIN)
				if(length(grp) != 1)
					throw "unexpected tokens after EXPLAIN: '[render_group(grp)]'"
				ret = TRUE
			else
				throw "unexpected group type '[type]' - code bug!"

		if(ret == TRUE || istype(ret, /list))
			tree[type] = ret
		else
			return ret

	return 0

/datum/sdql3/proc/render_group(list/grp)
	. = list()

	for(var/tok in grp)
		switch(tok[1])
			if(TYP_IDENTIFIER)
				. += tok[2][2]
			if(TYP_ARGLIST)
				var/list/show = list()
				for(var/a in tok[2])
					show[++show.len] = render_group(list(a))
				. += "(" + jointext(show, ", ") + ")"
			else
				. += tok[2]

	return jointext(., " ")

/datum/sdql3/proc/parse_select(list/grp)
	// Expected: "SELECT" expr ("," expr)*
	// Or:       "SELECT" "DISTINCT" expr ("," expr)*
	// Or:       "SELECT" "COUNT" "(" "*" ")"
	// Or:       "SELECT" "COUNT" "(" expr ")"
	// Or:       "SELECT" "*"

	if(length(grp) == 2 && TOK_IS(grp[2], TYP_KEYWORD, TOK_STAR))
		return list(TOK_STAR = TRUE)

	if(length(grp) == 5 && TOK_IS(grp[2], TYP_KEYWORD, TOK_COUNT) && TOK_IS(grp[3], TYP_KEYWORD, TOK_OPENBRKT) && TOK_IS(grp[length(grp)], TYP_KEYWORD, TOK_CLOSBRKT))
		if(TOK_IS(grp[4], TYP_KEYWORD, TOK_STAR))
			return list(TOK_COUNT = TRUE, COUNT_CONDITION = TRUE)
		else
			var/list/expr = parse_value(grp.Copy(4, length(grp)))
			if(istext(expr))
				return expr
			return list(TOK_COUNT = TRUE, COUNT_CONDITION = expr)

	var/list/select = list()

	if(TOK_IS(grp[2], TYP_KEYWORD, TOK_DISTINCT))
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

	select[SELECT_COLUMNS] = select_idents
	return select

/datum/sdql3/proc/parse_x_type(x, list/grp)
	// Expected: X type
	if(length(grp) != 2 || grp[2][1] != TYP_IDENTIFIER || grp[2][2][1] != IDTYP_LITERAL || (grp[2][2][2] != world && !ispath(grp[2][2][2])))
		return "expected a path to follow '[x]' at '[render_group(grp)]'"

	return list(grp[2][2])

/datum/sdql3/proc/parse_where(list/grp)
	// Expected: "WHERE" boolean-condition
	return parse_value(grp.Copy(2))

/datum/sdql3/proc/parse_set(list/grp)
	// Expected: "SET" ident "=" value ("," ident "=" value)*

	var/list/sets = list()
	var/list/set_vars = list()

	var/i = 1
	while(i <= length(grp))
		if(i+3 > length(grp))
			return "incomplete or invalid assignment at '[render_group(grp.Copy(i+1))]'"

		if(!TOK_IS(grp[i+2], TYP_KEYWORD, TOK_ASSIGN))
			return "missing '=' in assignment at '[render_group(grp.Copy(i+1))]'"

		if(grp[i+1][1] != TYP_IDENTIFIER || grp[i+1][2][1] != IDTYP_VARIABLE)
			return "left operand of '=' must be an identifier at '[render_group(grp.Copy(i+1))]'"

		var/ident = grp[i+1][2]
		var/str_ident = jointext(ident[2], ".")
		if(set_vars[str_ident])
			return "duplicated variable '[str_ident]' in '[render_group(grp)]'"

		var/comma = h_find_first_tok(grp, start=i+2, type=TOK_COMMA)
		if(comma == 0)
			comma = length(grp)+1

		var/list/expr = parse_value(grp.Copy(i+3, comma))
		if(istext(expr))
			return expr
		sets[ident] = expr
		set_vars[str_ident] = TRUE
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
		if(TOK_IS(bexpr[bi], TYP_KEYWORD, TOK_OPENBRKT))
			bi++
			ret[++ret.len] = brackets()
		else if(TOK_IS(bexpr[bi], TYP_KEYWORD, TOK_CLOSBRKT))
			bi++
			return reduce_expression(ret)
		else
			ret[++ret.len] = bexpr[bi]
			bi++
	return reduce_expression(ret)

/datum/sdql3/proc/reduce_expression(list/expr)
	group:
		while(length(expr) != 1)
			for(var/opgrp in sdql3_precedence)
				var/unary = sdql3_precedence[opgrp]
				for(var/op in opgrp)
					var/i = h_find_first_tok(expr, type=op)
					if(i != 0)
						if(!unary && i == 1)
							return "unexpected operator '[op]' with no left operand at '[render_group(expr)]'"
						if(i == length(expr))
							return "unexpected operator '[op]' with no right operand at '[render_group(expr)]'"

						if(unary && op == TOK_MINUS && i != 1) // special-case for -
							var/left = expr[i-1]
							if(left[1] != TYP_KEYWORD)
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

			if(length(expr) == 2 && expr[1][1] == TYP_IDENTIFIER && expr[1][2][1] == IDTYP_VARIABLE && expr[2][1] == TYP_ARGLIST)
				return list(TYP_CALL, expr[1][2][2], expr[2][2])

			if(length(expr) == 3 && TOK_IS(expr[1], TYP_KEYWORD, TOK_NEW))
				if(expr[2][1] != TYP_IDENTIFIER || expr[2][2][1] != IDTYP_LITERAL || !ispath(expr[2][2][2]))
					return "expected a type to follow 'new' at '[render_group(expr)]'"
				if(expr[3][1] != TYP_ARGLIST)
					return "expected an argument list to follow '[expr[2][2][2]]' at '[render_group(expr)]'"
				return list(TYP_NEW, expr[2][2][2], expr[3][2])

			if(length(expr) == 0 || (length(expr) % 2) == 1)
				var/list/args_if_valid = list()
				var/has_correct_commas = TRUE
				for(var/i in 1 to length(expr))
					if(i%2)
						// 1, 3, 5; these should be values
						if(expr[i][1] != TYP_IDENTIFIER)
							has_correct_commas = FALSE
							break
						args_if_valid[++args_if_valid.len] = expr[i]
					else
						// 2, 4, 6; these should be commas
						if(!TOK_IS(expr[i], TYP_KEYWORD, TOK_COMMA))
							has_correct_commas = FALSE
							break
				if(has_correct_commas)
					return list(TYP_ARGLIST, args_if_valid)

			return "invalid value expression at '[render_group(expr)]'"

	return list(TYP_EXPRESSION, expr[1])

/datum/sdql3/proc/render_expr(e)
	return json_encode(e)

/datum/sdql3/proc/h_find_first_tok(list/toks, start=1, type=TOK_COMMA, brackets=TRUE)
	var/brktlevel = 0
	for(var/i = start; i <= length(toks); i++)
		if(TOK_IS(toks[i], TYP_KEYWORD, TOK_OPENBRKT))
			brktlevel++
		else if(TOK_IS(toks[i], TYP_KEYWORD, TOK_CLOSBRKT))
			brktlevel--
		else if(TOK_IS(toks[i], TYP_KEYWORD, type))
			if(!brackets || brktlevel == 0)
				return i
	return 0

/datum/sdql3/proc/set_var(list/ident, value, datum/D)
	switch(ident[1])
		if("global")
			return set_global(ident.Copy(2), value, D)
		if("src")
			return set_datumvar(ident.Copy(2), value, D)
		else
			if(D && (ident[1] in D.vars))
				return set_datumvar(ident, value, D)
			if(ident[1] in global.vars)
				return set_global(ident, value)
			throw "attempt to [length(ident) == 1 ? "set" : "get"] nonexistent variable '[ident[1]]' on [D == world ? "world" : D.type]"

/datum/sdql3/proc/set_global(list/vname, value)
	if(!(vname[1] in global.vars))
		throw "attempt to [length(vname) == 1 ? "set" : "get"] nonexistent global variable '[vname[1]]'"

	if(length(vname) == 1)
		global.vars[vname[1]] = value // TODO access check
	else
		return set_datumvar(vname.Copy(2), value, global.vars[vname[1]]) // TODO access check

/datum/sdql3/proc/set_datumvar(list/vname, value, datum/D)
	if(!(vname[1] in D.vars))
		throw "attempt to [length(vname) == 1 ? "set" : "get"] nonexistent variable '[vname[1]]' on [D == world ? "world" : D.type]"

	if(length(vname) == 1)
		D.vars[vname[1]] = value // TODO access check
	else
		set_datumvar(vname.Copy(2), value, D.vars[vname[1]]) // TODO access check

/datum/sdql3/proc/get_ident(list/ident, datum/D)
	switch(ident[1])
		if(IDTYP_LITERAL) return ident[2]
		if(IDTYP_VARIABLE)
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
					throw "attempt to get nonexistent variable '[ident[2][1]]' on [D == world ? "world" : D.type]"
		else throw "invalid ident type [ident[1]]"

/datum/sdql3/proc/get_global(list/vname)
	if(!(vname[1] in global.vars))
		throw "attempt to get nonexistent global variable '[vname[1]]'"

	if(length(vname) == 1)
		return global.vars[vname[1]] // TODO access check
	else
		return get_var(vname.Copy(2), global.vars[vname[1]]) // TODO access check

/datum/sdql3/proc/get_var(list/vname, datum/D)
	if(!(vname[1] in D.vars))
		throw "attempt to get nonexistent variable '[vname[1]]' on [D == world ? "world" : D.type]"

	if(length(vname) == 1)
		return D.vars[vname[1]] // TODO access check
	else
		return get_var(vname.Copy(2), D.vars[vname[1]]) // TODO access check

/datum/sdql3/proc/eval_call(list/fn, list/ar, datum/D)
	var/function = fn[length(fn)]
	var/datum/base = D

	if(length(fn) == 2 && fn[1] == "global")
		function = "/proc/[function]"
		var/f = text2path(function)
		if(!f)
			throw "attempt to call nonexistent global function '[fn[2]]'"
		return call(f)(arglist(eval_exprs(ar, D)))

	if(length(fn) > 1)
		base = get_ident(list(IDTYP_VARIABLE, fn.Copy(1, length(fn))), D)

	if(hascall(base, function))
		return call(base, function)(arglist(eval_exprs(ar, D)))

	if(length(fn) == 1)
		var/g = text2path("/proc/[function]")
		if(g)
			return call(g)(arglist(eval_exprs(ar, D)))

		throw "attempt to call nonexistent function '[function]' in context of [base == world ? "world" : base.type]"
	else
		throw "attempt to call nonexistent function '[function]' on [base.type]"

/datum/sdql3/proc/eval_exprs(list/exprs, datum/D)
	var/list/ret = list()
	for(var/exp in exprs)
		ret[++ret.len] = eval_expr(exp, D)
	return ret

/datum/sdql3/proc/eval_expr(list/tree, datum/D)
	switch(tree[1])
		if(TYP_IDENTIFIER)
			return get_ident(tree[2], D)
		if(TYP_EXPRESSION)
			return eval_expr(tree[2], D)
		if(TYP_CALL)
			return eval_call(tree[2], tree[3], D)
		if(TYP_NEW)
			var/typ = tree[2]
			return new typ(arglist(eval_exprs(tree[3])))

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
			throw "invalid operator [tree[1]] - code bug!"

/datum/sdql3/proc/execute()
	if(TOK_EXPLAIN in tree)
		tree -= TOK_EXPLAIN
		return json_encode(tree)

	else if(TOK_SELECT in tree)
		var/select = tree[TOK_SELECT]

		var/list/things = list(null)
		if(TOK_FROM in tree)
			things = get_from(tree[TOK_FROM][1][2])

		if(TOK_WHERE in tree)
			var/where = tree[TOK_WHERE]
			for(var/thing in things)
				if(!eval_expr(where, thing))
					things -= thing

		if(select[TOK_COUNT])
			var/expr = select[COUNT_CONDITION]
			if(expr == TRUE)
				// SELECT COUNT(*)
				return length(things)
			else
				// SELECT COUNT(expr)
				var/count = 0
				for(var/thing in things)
					if(!isnull(eval_expr(expr, thing)))
						count++
				return count

		if(select[TOK_STAR])
			// SELECT *
			return things

		else if(select[TOK_DISTINCT])
			// SELECT DISTINCT ident, ident, ident
			var/list/values = list()

			var/list/idents = select[SELECT_COLUMNS]
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
			var/list/idents = select[SELECT_COLUMNS]

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
			var/values = list()
			for(var/ident in update)
				values[ident] = eval_expr(update[ident], thing)

			for(var/ident in values)
				set_var(ident[2], values[ident], thing)

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

	else
		throw "unknown expression [json_encode(tree)] - code bug!"


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

/datum/sdql3/proc/get_from(type)
	if(type == world)
		return list(world)

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
	if(isnum(v) || istext(v))
		return json_encode(v)
	return v

/proc/k2v(k)
	if(istext(k))
		return json_decode(k)
	return k
