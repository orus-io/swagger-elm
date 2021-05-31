(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.M.D === region.T.D)
	{
		return 'on line ' + region.M.D;
	}
	return 'on lines ' + region.M.D + ' through ' + region.T.D;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);


// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.aT) { flags += 'm'; }
	if (options.aG) { flags += 'i'; }

	try
	{
		return $elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return $elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		out.push(A4($elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		return replacer(A4($elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.aQ,
		impl.a1,
		impl.a$,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.e) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.g),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.g);
		} else {
			var treeLen = builder.e * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.h) : builder.h;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.e);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.g) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.g);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{h: nodeList, e: (len / $elm$core$Array$branchFactor) | 0, g: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $author$project$Swagger$Swagger$Swagger = function (definitions) {
	return {aJ: definitions};
};
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $author$project$Swagger$Decode$decode = $elm$json$Json$Decode$succeed;
var $author$project$Swagger$Type$Array_ = function (a) {
	return {$: 1, a: a};
};
var $author$project$Swagger$Type$Bool_ = function (a) {
	return {$: 7, a: a};
};
var $author$project$Swagger$Type$Float_ = function (a) {
	return {$: 6, a: a};
};
var $author$project$Swagger$Type$Int_ = function (a) {
	return {$: 5, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $author$project$Swagger$Type$Items = $elm$core$Basics$identity;
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Swagger$Decode$decodeAlwaysString = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			$elm$json$Json$Decode$string,
			A2($elm$json$Json$Decode$map, $elm$core$String$fromInt, $elm$json$Json$Decode$int),
			A2($elm$json$Json$Decode$map, $elm$core$String$fromFloat, $elm$json$Json$Decode$float),
			A2(
			$elm$json$Json$Decode$map,
			function (b) {
				return b ? 'true' : 'false';
			},
			$elm$json$Json$Decode$bool)
		]));
var $elm$json$Json$Decode$map2 = _Json_map2;
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom = $elm$json$Json$Decode$map2($elm$core$Basics$apR);
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optionalDecoder = F3(
	function (pathDecoder, valDecoder, fallback) {
		var nullOr = function (decoder) {
			return $elm$json$Json$Decode$oneOf(
				_List_fromArray(
					[
						decoder,
						$elm$json$Json$Decode$null(fallback)
					]));
		};
		var handleResult = function (input) {
			var _v0 = A2($elm$json$Json$Decode$decodeValue, pathDecoder, input);
			if (!_v0.$) {
				var rawValue = _v0.a;
				var _v1 = A2(
					$elm$json$Json$Decode$decodeValue,
					nullOr(valDecoder),
					rawValue);
				if (!_v1.$) {
					var finalResult = _v1.a;
					return $elm$json$Json$Decode$succeed(finalResult);
				} else {
					var finalErr = _v1.a;
					return $elm$json$Json$Decode$fail(
						$elm$json$Json$Decode$errorToString(finalErr));
				}
			} else {
				return $elm$json$Json$Decode$succeed(fallback);
			}
		};
		return A2($elm$json$Json$Decode$andThen, handleResult, $elm$json$Json$Decode$value);
	});
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional = F4(
	function (key, valDecoder, fallback, decoder) {
		return A2(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optionalDecoder,
				A2($elm$json$Json$Decode$field, key, $elm$json$Json$Decode$value),
				valDecoder,
				fallback),
			decoder);
	});
var $author$project$Swagger$Decode$maybe = F2(
	function (name, decoder) {
		return A3(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
			name,
			A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder),
			$elm$core$Maybe$Nothing);
	});
var $author$project$Swagger$Decode$decodePrimitive = function (constructor) {
	return A2(
		$elm$json$Json$Decode$map,
		constructor,
		A3(
			$author$project$Swagger$Decode$maybe,
			'default',
			$author$project$Swagger$Decode$decodeAlwaysString,
			$author$project$Swagger$Decode$decode($elm$core$Basics$identity)));
};
var $author$project$Swagger$Type$Ref_ = function (a) {
	return {$: 8, a: a};
};
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {aP: index, aS: match, aV: number, a_: submatches};
	});
var $elm$regex$Regex$findAtMost = _Regex_findAtMost;
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var $elm$regex$Regex$fromString = function (string) {
	return A2(
		$elm$regex$Regex$fromStringWith,
		{aG: false, aT: false},
		string);
};
var $elm$regex$Regex$never = _Regex_never;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Swagger$Decode$regex = A2(
	$elm$core$Basics$composeR,
	$elm$regex$Regex$fromString,
	$elm$core$Maybe$withDefault($elm$regex$Regex$never));
var $author$project$Swagger$Decode$extractRef = function (ref) {
	var parsed = A2(
		$elm$core$Maybe$andThen,
		A2(
			$elm$core$Basics$composeL,
			$elm$core$List$head,
			function ($) {
				return $.a_;
			}),
		$elm$core$List$head(
			A3(
				$elm$regex$Regex$findAtMost,
				1,
				$author$project$Swagger$Decode$regex('^#/definitions/(.+)$'),
				ref)));
	if ((!parsed.$) && (!parsed.a.$)) {
		var ref_ = parsed.a.a;
		return $elm$json$Json$Decode$succeed(ref_);
	} else {
		return $elm$json$Json$Decode$fail('Unparseable reference ' + ref);
	}
};
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required = F3(
	function (key, valDecoder, decoder) {
		return A2(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
			A2($elm$json$Json$Decode$field, key, valDecoder),
			decoder);
	});
var $author$project$Swagger$Decode$decodeRef = A2(
	$elm$json$Json$Decode$map,
	$author$project$Swagger$Type$Ref_,
	A2(
		$elm$json$Json$Decode$andThen,
		$author$project$Swagger$Decode$extractRef,
		A3(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'$ref',
			$elm$json$Json$Decode$string,
			$author$project$Swagger$Decode$decode($elm$core$Basics$identity))));
var $author$project$Swagger$Decode$apply2 = F2(
	function (fn, _v0) {
		var a = _v0.a;
		var b = _v0.b;
		return A2(fn, a, b);
	});
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $author$project$Swagger$Type$Enum_ = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $author$project$Swagger$Type$String_ = function (a) {
	return {$: 3, a: a};
};
var $author$project$Swagger$Decode$stringOrEnum = F2(
	function (_default, _enum) {
		if (_enum.$ === 1) {
			return $author$project$Swagger$Type$String_(_default);
		} else {
			var value = _enum.a;
			return A2($author$project$Swagger$Type$Enum_, _default, value);
		}
	});
var $author$project$Swagger$Decode$decodeString = A2(
	$elm$json$Json$Decode$map,
	$author$project$Swagger$Decode$apply2($author$project$Swagger$Decode$stringOrEnum),
	A3(
		$author$project$Swagger$Decode$maybe,
		'enum',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
		A3(
			$author$project$Swagger$Decode$maybe,
			'default',
			$author$project$Swagger$Decode$decodeAlwaysString,
			$author$project$Swagger$Decode$decode($elm$core$Tuple$pair))));
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm$json$Json$Decode$lazy = function (thunk) {
	return A2(
		$elm$json$Json$Decode$andThen,
		thunk,
		$elm$json$Json$Decode$succeed(0));
};
var $author$project$Swagger$Type$Dict_ = function (a) {
	return {$: 2, a: a};
};
var $author$project$Swagger$Type$Object_ = function (a) {
	return {$: 0, a: a};
};
var $author$project$Swagger$Type$Properties = $elm$core$Basics$identity;
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $author$project$Swagger$Type$Default = F3(
	function (a, b, c) {
		return {$: 2, a: a, b: b, c: c};
	});
var $author$project$Swagger$Type$Optional = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$Swagger$Type$Required = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $author$project$Swagger$Type$getDefault = function (type_) {
	_v0$5:
	while (true) {
		switch (type_.$) {
			case 3:
				if (!type_.a.$) {
					var _default = type_.a.a;
					return $elm$core$Maybe$Just(_default);
				} else {
					break _v0$5;
				}
			case 5:
				if (!type_.a.$) {
					var _default = type_.a.a;
					return $elm$core$Maybe$Just(_default);
				} else {
					break _v0$5;
				}
			case 6:
				if (!type_.a.$) {
					var _default = type_.a.a;
					return $elm$core$Maybe$Just(_default);
				} else {
					break _v0$5;
				}
			case 7:
				if (!type_.a.$) {
					var _default = type_.a.a;
					return $elm$core$Maybe$Just(_default);
				} else {
					break _v0$5;
				}
			case 4:
				if (!type_.a.$) {
					var _default = type_.a.a;
					return $elm$core$Maybe$Just(_default);
				} else {
					break _v0$5;
				}
			default:
				break _v0$5;
		}
	}
	return $elm$core$Maybe$Nothing;
};
var $author$project$Swagger$Decode$property = F2(
	function (required, _v0) {
		var name = _v0.a;
		var type_ = _v0.b;
		var _v1 = A2(
			$elm$core$List$any,
			$elm$core$Basics$eq(name),
			required);
		if (_v1) {
			return A2($author$project$Swagger$Type$Required, name, type_);
		} else {
			var _v2 = $author$project$Swagger$Type$getDefault(type_);
			if (!_v2.$) {
				var _default = _v2.a;
				return A3($author$project$Swagger$Type$Default, name, type_, _default);
			} else {
				return A2($author$project$Swagger$Type$Optional, name, type_);
			}
		}
	});
var $author$project$Swagger$Decode$objectOrDict = function (_v0) {
	var required = _v0.a;
	var properties = _v0.b;
	var additionalProperties = _v0.c;
	var _v1 = _Utils_Tuple2(properties, additionalProperties);
	if (_v1.a.$ === 1) {
		if (!_v1.b.$) {
			var _v2 = _v1.a;
			var addProps = _v1.b.a;
			return $author$project$Swagger$Type$Dict_(addProps);
		} else {
			return $author$project$Swagger$Type$Object_(_List_Nil);
		}
	} else {
		var props = _v1.a.a;
		return $author$project$Swagger$Type$Object_(
			A2(
				$elm$core$List$map,
				$author$project$Swagger$Decode$property(required),
				props));
	}
};
var $author$project$Swagger$Decode$trio = F3(
	function (a, b, c) {
		return _Utils_Tuple3(a, b, c);
	});
var $author$project$Swagger$Decode$decodeTypeByType = function (_v0) {
	var type_ = _v0.a;
	var ref = _v0.b;
	if (!ref.$) {
		var ref_ = ref.a;
		return $author$project$Swagger$Decode$decodeRef;
	} else {
		switch (type_) {
			case 'string':
				return $author$project$Swagger$Decode$decodeString;
			case 'integer':
				return $author$project$Swagger$Decode$decodePrimitive($author$project$Swagger$Type$Int_);
			case 'number':
				return $author$project$Swagger$Decode$decodePrimitive($author$project$Swagger$Type$Float_);
			case 'boolean':
				return $author$project$Swagger$Decode$decodePrimitive($author$project$Swagger$Type$Bool_);
			case 'array':
				return $author$project$Swagger$Decode$cyclic$decodeArray();
			default:
				return $author$project$Swagger$Decode$cyclic$decodeObject();
		}
	}
};
function $author$project$Swagger$Decode$cyclic$decodeArray() {
	return A2(
		$elm$json$Json$Decode$map,
		A2($elm$core$Basics$composeL, $author$project$Swagger$Type$Array_, $elm$core$Basics$identity),
		A3(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'items',
			$elm$json$Json$Decode$lazy(
				function (_v6) {
					return $author$project$Swagger$Decode$cyclic$decodeType();
				}),
			$author$project$Swagger$Decode$decode($elm$core$Basics$identity)));
}
function $author$project$Swagger$Decode$cyclic$decodeObject() {
	return A2(
		$elm$json$Json$Decode$map,
		$author$project$Swagger$Decode$objectOrDict,
		A3(
			$author$project$Swagger$Decode$maybe,
			'additionalProperties',
			$elm$json$Json$Decode$lazy(
				function (_v5) {
					return $author$project$Swagger$Decode$cyclic$decodeType();
				}),
			A3(
				$author$project$Swagger$Decode$maybe,
				'properties',
				$elm$json$Json$Decode$lazy(
					function (_v4) {
						return $elm$json$Json$Decode$keyValuePairs(
							$author$project$Swagger$Decode$cyclic$decodeType());
					}),
				A4(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
					'required',
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
					_List_Nil,
					$author$project$Swagger$Decode$decode($author$project$Swagger$Decode$trio)))));
}
function $author$project$Swagger$Decode$cyclic$decodeType() {
	return $elm$json$Json$Decode$lazy(
		function (_v3) {
			return A2(
				$elm$json$Json$Decode$andThen,
				$author$project$Swagger$Decode$decodeTypeByType,
				A3(
					$author$project$Swagger$Decode$maybe,
					'$ref',
					$elm$json$Json$Decode$string,
					A4(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
						'type',
						$elm$json$Json$Decode$string,
						'',
						$author$project$Swagger$Decode$decode($elm$core$Tuple$pair))));
		});
}
var $author$project$Swagger$Decode$decodeArray = $author$project$Swagger$Decode$cyclic$decodeArray();
$author$project$Swagger$Decode$cyclic$decodeArray = function () {
	return $author$project$Swagger$Decode$decodeArray;
};
var $author$project$Swagger$Decode$decodeObject = $author$project$Swagger$Decode$cyclic$decodeObject();
$author$project$Swagger$Decode$cyclic$decodeObject = function () {
	return $author$project$Swagger$Decode$decodeObject;
};
var $author$project$Swagger$Decode$decodeType = $author$project$Swagger$Decode$cyclic$decodeType();
$author$project$Swagger$Decode$cyclic$decodeType = function () {
	return $author$project$Swagger$Decode$decodeType;
};
var $author$project$Swagger$Definition$Definition = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Swagger$Definition$NestedDefinition = F3(
	function (a, b, c) {
		return {$: 1, a: a, b: b, c: c};
	});
var $author$project$Swagger$Definition$definition = F3(
	function (parentNames, name, type_) {
		if (parentNames.$ === 1) {
			return A2($author$project$Swagger$Definition$Definition, name, type_);
		} else {
			var parents = parentNames.a;
			return A3($author$project$Swagger$Definition$NestedDefinition, parents, name, type_);
		}
	});
var $author$project$Swagger$Definition$Definitions = $elm$core$Basics$identity;
var $author$project$Swagger$Definition$definitions = function (defs) {
	return defs;
};
var $author$project$Swagger$Decode$decodeTypes = A2(
	$elm$json$Json$Decode$map,
	$author$project$Swagger$Definition$definitions,
	A2(
		$elm$json$Json$Decode$map,
		$elm$core$List$map(
			function (_v0) {
				var name = _v0.a;
				var type_ = _v0.b;
				return A3($author$project$Swagger$Definition$definition, $elm$core$Maybe$Nothing, name, type_);
			}),
		$elm$json$Json$Decode$keyValuePairs($author$project$Swagger$Decode$decodeType)));
var $author$project$Swagger$Decode$decodeSwagger = A3(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'definitions',
	$author$project$Swagger$Decode$decodeTypes,
	$author$project$Swagger$Decode$decode($author$project$Swagger$Swagger$Swagger));
var $author$project$Swagger$Type$getPropertyName = function (prop) {
	switch (prop.$) {
		case 0:
			var name = prop.a;
			return name;
		case 1:
			var name = prop.a;
			return name;
		default:
			var name = prop.a;
			return name;
	}
};
var $author$project$Swagger$Type$getPropertyType = function (prop) {
	switch (prop.$) {
		case 0:
			var type_ = prop.b;
			return type_;
		case 1:
			var type_ = prop.b;
			return type_;
		default:
			var type_ = prop.b;
			return type_;
	}
};
var $author$project$Swagger$Definition$prepend = F2(
	function (def, _v0) {
		var defs = _v0;
		return A2($elm$core$List$cons, def, defs);
	});
var $author$project$Swagger$Flatten$flattenItems = F3(
	function (parentNames, _v2, definitions) {
		var type_ = _v2;
		return A4($author$project$Swagger$Flatten$flattenType, parentNames, 'Item', type_, definitions);
	});
var $author$project$Swagger$Flatten$flattenProperties = F3(
	function (parentNames, _v1, definitions) {
		var props = _v1;
		return A3(
			$elm$core$List$foldl,
			$author$project$Swagger$Flatten$flattenProperty(parentNames),
			definitions,
			props);
	});
var $author$project$Swagger$Flatten$flattenProperty = F3(
	function (parentNames, prop, definitions) {
		return A4(
			$author$project$Swagger$Flatten$flattenType,
			parentNames,
			$author$project$Swagger$Type$getPropertyName(prop),
			$author$project$Swagger$Type$getPropertyType(prop),
			definitions);
	});
var $author$project$Swagger$Flatten$flattenType = F4(
	function (parentNames, name, type_, definitions) {
		flattenType:
		while (true) {
			var prependSelf = $author$project$Swagger$Definition$prepend(
				A3(
					$author$project$Swagger$Definition$definition,
					$elm$core$Maybe$Just(parentNames),
					name,
					type_));
			var childParentNames = A2($elm$core$List$cons, name, parentNames);
			switch (type_.$) {
				case 0:
					var props = type_.a;
					return prependSelf(
						A3($author$project$Swagger$Flatten$flattenProperties, childParentNames, props, definitions));
				case 1:
					var items = type_.a;
					return prependSelf(
						A3($author$project$Swagger$Flatten$flattenItems, childParentNames, items, definitions));
				case 2:
					var typename = type_.a;
					var $temp$parentNames = childParentNames,
						$temp$name = 'Property',
						$temp$type_ = typename,
						$temp$definitions = definitions;
					parentNames = $temp$parentNames;
					name = $temp$name;
					type_ = $temp$type_;
					definitions = $temp$definitions;
					continue flattenType;
				case 4:
					return prependSelf(definitions);
				case 3:
					return definitions;
				case 5:
					return definitions;
				case 6:
					return definitions;
				case 7:
					return definitions;
				default:
					return definitions;
			}
		}
	});
var $author$project$Swagger$Definition$getName = function (def) {
	if (!def.$) {
		var name = def.a;
		return name;
	} else {
		var name = def.b;
		return name;
	}
};
var $author$project$Swagger$Definition$getType = function (def) {
	if (!def.$) {
		var type_ = def.b;
		return type_;
	} else {
		var type_ = def.c;
		return type_;
	}
};
var $author$project$Swagger$Flatten$flattenEachRoot = F2(
	function (definition, definitions) {
		var name = $author$project$Swagger$Definition$getName(definition);
		var newDefinitions = function () {
			var _v0 = $author$project$Swagger$Definition$getType(definition);
			switch (_v0.$) {
				case 0:
					var props = _v0.a;
					return A3(
						$author$project$Swagger$Flatten$flattenProperties,
						_List_fromArray(
							[name]),
						props,
						definitions);
				case 1:
					var items = _v0.a;
					return A3(
						$author$project$Swagger$Flatten$flattenItems,
						_List_fromArray(
							[name]),
						items,
						definitions);
				case 2:
					var type_ = _v0.a;
					return A4(
						$author$project$Swagger$Flatten$flattenType,
						_List_fromArray(
							[name]),
						'Property',
						type_,
						definitions);
				case 4:
					return definitions;
				case 3:
					return definitions;
				case 5:
					return definitions;
				case 6:
					return definitions;
				case 7:
					return definitions;
				default:
					return definitions;
			}
		}();
		return A2($author$project$Swagger$Definition$prepend, definition, newDefinitions);
	});
var $author$project$Swagger$Definition$foldl = F3(
	function (fn, init, _v0) {
		var defs = _v0;
		return A3($elm$core$List$foldl, fn, init, defs);
	});
var $author$project$Swagger$Definition$singleton = _List_Nil;
var $author$project$Swagger$Flatten$flattenDefinitions = A2($author$project$Swagger$Definition$foldl, $author$project$Swagger$Flatten$flattenEachRoot, $author$project$Swagger$Definition$singleton);
var $author$project$Swagger$Flatten$flatten = function (swagger) {
	var definitions = swagger.aJ;
	return _Utils_update(
		swagger,
		{
			aJ: $author$project$Swagger$Flatten$flattenDefinitions(definitions)
		});
};
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (!ra.$) {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (!result.$) {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $author$project$Swagger$Definition$map = F2(
	function (fn, _v0) {
		var list = _v0;
		return A2($elm$core$List$map, fn, list);
	});
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$String$toUpper = _String_toUpper;
var $author$project$Codegen$Utils$capitalize = function (str) {
	var _v0 = $elm$core$String$uncons(str);
	if (!_v0.$) {
		var _v1 = _v0.a;
		var head = _v1.a;
		var tail = _v1.b;
		return _Utils_ap(
			$elm$core$String$toUpper(
				$elm$core$String$fromChar(head)),
			tail);
	} else {
		return '';
	}
};
var $elm$regex$Regex$contains = _Regex_contains;
var $author$project$Codegen$Utils$regex = A2(
	$elm$core$Basics$composeR,
	$elm$regex$Regex$fromString,
	$elm$core$Maybe$withDefault($elm$regex$Regex$never));
var $author$project$Codegen$Utils$validFirstUnicodeCharacters = 'A-ZÀ-ÖØ-ÞĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞĠĢĤĦĨĪĬĮİĲĴĶĹĻĽĿŁŃŅŇŊŌŎŐŒŔŖŘŚŜŞŠŢŤŦŨŪŬŮŰŲŴŶŸŹŻŽƁƂƄƆƇƉ-ƋƎ-ƑƓƔƖ-ƘƜƝƟƠƢƤƦƧƩƬƮƯƱ-ƳƵƷƸƼǄǇǊǍǏǑǓǕǗǙǛǞǠǢǤǦǨǪǬǮǱǴǶ-ǸǺǼǾȀȂȄȆȈȊȌȎȐȒȔȖȘȚȜȞȠȢȤȦȨȪȬȮȰȲȺȻȽȾɁɃ-ɆɈɊɌɎͰͲͶΆΈ-ΊΌΎΏΑ-ΡΣ-ΫϏϒ-ϔϘϚϜϞϠϢϤϦϨϪϬϮϴϷϹϺϽ-ЯѠѢѤѦѨѪѬѮѰѲѴѶѸѺѼѾҀҊҌҎҐҒҔҖҘҚҜҞҠҢҤҦҨҪҬҮҰҲҴҶҸҺҼҾӀӁӃӅӇӉӋӍӐӒӔӖӘӚӜӞӠӢӤӦӨӪӬӮӰӲӴӶӸӺӼӾԀԂԄԆԈԊԌԎԐԒԔԖԘԚԜԞԠԢԤԦԱ-ՖႠ-ჅḀḂḄḆḈḊḌḎḐḒḔḖḘḚḜḞḠḢḤḦḨḪḬḮḰḲḴḶḸḺḼḾṀṂṄṆṈṊṌṎṐṒṔṖṘṚṜṞṠṢṤṦṨṪṬṮṰṲṴṶṸṺṼṾẀẂẄẆẈẊẌẎẐẒẔẞẠẢẤẦẨẪẬẮẰẲẴẶẸẺẼẾỀỂỄỆỈỊỌỎỐỒỔỖỘỚỜỞỠỢỤỦỨỪỬỮỰỲỴỶỸỺỼỾἈ-ἏἘ-ἝἨ-ἯἸ-ἿὈ-ὍὙὛὝὟὨ-ὯᾸ-ΆῈ-ΉῘ-ΊῨ-ῬῸ-Ώℂℇℋ-ℍℐ-ℒℕℙ-ℝℤΩℨK-ℭℰ-ℳℾℿⅅↃⰀ-ⰮⱠⱢ-ⱤⱧⱩⱫⱭ-ⱰⱲⱵⱾ-ⲀⲂⲄⲆⲈⲊⲌⲎⲐⲒⲔⲖⲘⲚⲜⲞⲠⲢⲤⲦⲨⲪⲬⲮⲰⲲⲴⲶⲸⲺⲼⲾⳀⳂⳄⳆⳈⳊⳌⳎⳐⳒⳔⳖⳘⳚⳜⳞⳠⳢⳫⳭꙀꙂꙄꙆꙈꙊꙌꙎꙐꙒꙔꙖꙘꙚꙜꙞꙠꙢꙤꙦꙨꙪꙬꚀꚂꚄꚆꚈꚊꚌꚎꚐꚒꚔꚖꜢꜤꜦꜨꜪꜬꜮꜲꜴꜶꜸꜺꜼꜾꝀꝂꝄꝆꝈꝊꝌꝎꝐꝒꝔꝖꝘꝚꝜꝞꝠꝢꝤꝦꝨꝪꝬꝮꝹꝻꝽꝾꞀꞂꞄꞆꞋꞍꞐꞠꞢꞤꞦꞨＡ-Ｚa-zªµºß-öø-ÿāăąćĉċčďđēĕėęěĝğġģĥħĩīĭįıĳĵķĸĺļľŀłńņňŉŋōŏőœŕŗřśŝşšţťŧũūŭůűųŵŷźżž-ƀƃƅƈƌƍƒƕƙ-ƛƞơƣƥƨƪƫƭưƴƶƹƺƽ-ƿǆǉǌǎǐǒǔǖǘǚǜǝǟǡǣǥǧǩǫǭǯǰǳǵǹǻǽǿȁȃȅȇȉȋȍȏȑȓȕȗșțȝȟȡȣȥȧȩȫȭȯȱȳ-ȹȼȿɀɂɇɉɋɍɏ-ʓʕ-ʯͱͳͷͻ-ͽΐά-ώϐϑϕ-ϗϙϛϝϟϡϣϥϧϩϫϭϯ-ϳϵϸϻϼа-џѡѣѥѧѩѫѭѯѱѳѵѷѹѻѽѿҁҋҍҏґғҕҗҙқҝҟҡңҥҧҩҫҭүұҳҵҷҹһҽҿӂӄӆӈӊӌӎӏӑӓӕӗәӛӝӟӡӣӥӧөӫӭӯӱӳӵӷӹӻӽӿԁԃԅԇԉԋԍԏԑԓԕԗԙԛԝԟԡԣԥԧա-ևᴀ-ᴫᵢ-ᵷᵹ-ᶚḁḃḅḇḉḋḍḏḑḓḕḗḙḛḝḟḡḣḥḧḩḫḭḯḱḳḵḷḹḻḽḿṁṃṅṇṉṋṍṏṑṓṕṗṙṛṝṟṡṣṥṧṩṫṭṯṱṳṵṷṹṻṽṿẁẃẅẇẉẋẍẏẑẓẕ-ẝẟạảấầẩẫậắằẳẵặẹẻẽếềểễệỉịọỏốồổỗộớờởỡợụủứừửữựỳỵỷỹỻỽỿ-ἇἐ-ἕἠ-ἧἰ-ἷὀ-ὅὐ-ὗὠ-ὧὰ-ώᾀ-ᾇᾐ-ᾗᾠ-ᾧᾰ-ᾴᾶᾷιῂ-ῄῆῇῐ-ΐῖῗῠ-ῧῲ-ῴῶῷℊℎℏℓℯℴℹℼℽⅆ-ⅉⅎↄⰰ-ⱞⱡⱥⱦⱨⱪⱬⱱⱳⱴⱶ-ⱼⲁⲃⲅⲇⲉⲋⲍⲏⲑⲓⲕⲗⲙⲛⲝⲟⲡⲣⲥⲧⲩⲫⲭⲯⲱⲳⲵⲷⲹⲻⲽⲿⳁⳃⳅⳇⳉⳋⳍⳏⳑⳓⳕⳗⳙⳛⳝⳟⳡⳣⳤⳬⳮⴀ-ⴥꙁꙃꙅꙇꙉꙋꙍꙏꙑꙓꙕꙗꙙꙛꙝꙟꙡꙣꙥꙧꙩꙫꙭꚁꚃꚅꚇꚉꚋꚍꚏꚑꚓꚕꚗꜣꜥꜧꜩꜫꜭꜯ-ꜱꜳꜵꜷꜹꜻꜽꜿꝁꝃꝅꝇꝉꝋꝍꝏꝑꝓꝕꝗꝙꝛꝝꝟꝡꝣꝥꝧꝩꝫꝭꝯꝱ-ꝸꝺꝼꝿꞁꞃꞅꞇꞌꞎꞑꞡꞣꞥꞧꞩꟺﬀ-ﬆﬓ-ﬗａ-ｚǅǈǋǲᾈ-ᾏᾘ-ᾟᾨ-ᾯᾼῌῼ';
var $author$project$Codegen$Utils$validLeadingChars = '[' + ($author$project$Codegen$Utils$validFirstUnicodeCharacters + ']');
var $author$project$Codegen$Utils$isValidFirst = function (str) {
	return A2(
		$elm$regex$Regex$contains,
		$author$project$Codegen$Utils$regex($author$project$Codegen$Utils$validLeadingChars),
		$elm$core$String$fromChar(str));
};
var $author$project$Codegen$Utils$sanitizeFirst = function (str) {
	sanitizeFirst:
	while (true) {
		var _v0 = $elm$core$String$uncons(str);
		if (!_v0.$) {
			var _v1 = _v0.a;
			var head = _v1.a;
			var tail = _v1.b;
			if ($author$project$Codegen$Utils$isValidFirst(head)) {
				return A2($elm$core$String$cons, head, tail);
			} else {
				var $temp$str = tail;
				str = $temp$str;
				continue sanitizeFirst;
			}
		} else {
			return '';
		}
	}
};
var $elm$regex$Regex$find = _Regex_findAtMost(_Regex_infinity);
var $author$project$Codegen$Utils$validRestUnicodeCharacters = $author$project$Codegen$Utils$validFirstUnicodeCharacters + 'ʰ-ˁˆ-ˑˠ-ˤˬˮʹͺՙـۥۦߴߵߺࠚࠤࠨॱๆໆჼៗᡃᪧᱸ-ᱽᴬ-ᵡᵸᶛ-ᶿⁱⁿₐ-ₜⱽⵯⸯ々〱-〵〻ゝゞー-ヾꀕꓸ-ꓽꘌꙿꜗ-ꜟꝰꞈꧏꩰꫝｰﾞﾟƻǀ-ǃʔא-תװ-ײؠ-ؿف-يٮٯٱ-ۓەۮۯۺ-ۼۿܐܒ-ܯݍ-ޥޱߊ-ߪࠀ-ࠕࡀ-ࡘऄ-हऽॐक़-ॡॲ-ॷॹ-ॿঅ-ঌএঐও-নপ-রলশ-হঽৎড়ঢ়য়-ৡৰৱਅ-ਊਏਐਓ-ਨਪ-ਰਲਲ਼ਵਸ਼ਸਹਖ਼-ੜਫ਼ੲ-ੴઅ-ઍએ-ઑઓ-નપ-રલળવ-હઽૐૠૡଅ-ଌଏଐଓ-ନପ-ରଲଳଵ-ହଽଡ଼ଢ଼ୟ-ୡୱஃஅ-ஊஎ-ஐஒ-கஙசஜஞடணதந-பம-ஹௐఅ-ఌఎ-ఐఒ-నప-ళవ-హఽౘౙౠౡಅ-ಌಎ-ಐಒ-ನಪ-ಳವ-ಹಽೞೠೡೱೲഅ-ഌഎ-ഐഒ-ഺഽൎൠൡൺ-ൿඅ-ඖක-නඳ-රලව-ෆก-ะาำเ-ๅກຂຄງຈຊຍດ-ທນ-ຟມ-ຣລວສຫອ-ະາຳຽເ-ໄໜໝༀཀ-ཇཉ-ཬྈ-ྌက-ဪဿၐ-ၕၚ-ၝၡၥၦၮ-ၰၵ-ႁႎა-ჺᄀ-ቈቊ-ቍቐ-ቖቘቚ-ቝበ-ኈኊ-ኍነ-ኰኲ-ኵኸ-ኾዀዂ-ዅወ-ዖዘ-ጐጒ-ጕጘ-ፚᎀ-ᎏᎠ-Ᏼᐁ-ᙬᙯ-ᙿᚁ-ᚚᚠ-ᛪᜀ-ᜌᜎ-ᜑᜠ-ᜱᝀ-ᝑᝠ-ᝬᝮ-ᝰក-ឳៜᠠ-ᡂᡄ-ᡷᢀ-ᢨᢪᢰ-ᣵᤀ-ᤜᥐ-ᥭᥰ-ᥴᦀ-ᦫᧁ-ᧇᨀ-ᨖᨠ-ᩔᬅ-ᬳᭅ-ᭋᮃ-ᮠᮮᮯᯀ-ᯥᰀ-ᰣᱍ-ᱏᱚ-ᱷᳩ-ᳬᳮ-ᳱℵ-ℸⴰ-ⵥⶀ-ⶖⶠ-ⶦⶨ-ⶮⶰ-ⶶⶸ-ⶾⷀ-ⷆⷈ-ⷎⷐ-ⷖⷘ-ⷞ〆〼ぁ-ゖゟァ-ヺヿㄅ-ㄭㄱ-ㆎㆠ-ㆺㇰ-ㇿ㐀-䶵一-鿋ꀀ-ꀔꀖ-ꒌꓐ-ꓷꔀ-ꘋꘐ-ꘟꘪꘫꙮꚠ-ꛥꟻ-ꠁꠃ-ꠅꠇ-ꠊꠌ-ꠢꡀ-ꡳꢂ-ꢳꣲ-ꣷꣻꤊ-ꤥꤰ-ꥆꥠ-ꥼꦄ-ꦲꨀ-ꨨꩀ-ꩂꩄ-ꩋꩠ-ꩯꩱ-ꩶꩺꪀ-ꪯꪱꪵꪶꪹ-ꪽꫀꫂꫛꫜꬁ-ꬆꬉ-ꬎꬑ-ꬖꬠ-ꬦꬨ-ꬮꯀ-ꯢ가-힣ힰ-ퟆퟋ-ퟻ豈-鶴侮-舘並-龎יִײַ-ﬨשׁ-זּטּ-לּמּנּסּףּפּצּ-ﮱﯓ-ﴽﵐ-ﶏﶒ-ﷇﷰ-ﷻﹰ-ﹴﹶ-ﻼｦ-ｯｱ-ﾝﾠ-ﾾￂ-ￇￊ-ￏￒ-ￗￚ-ￜ';
var $author$project$Codegen$Utils$validUnicodeNumerals = '0-9٠-٩۰-۹߀-߉०-९০-৯੦-੯૦-૯୦-୯௦-௯౦-౯೦-೯൦-൯๐-๙໐-໙༠-༩၀-၉႐-႙០-៩᠐-᠙᥆-᥏᧐-᧙᪀-᪉᪐-᪙᭐-᭙᮰-᮹᱀-᱉᱐-᱙꘠-꘩꣐-꣙꤀-꤉꧐-꧙꩐-꩙꯰-꯹０-９ᛮ-ᛰⅠ-ↂↅ-ↈ〇〡-〩〸-〺ꛦ-ꛯ';
var $author$project$Codegen$Utils$validUnicodeSymbols = '_';
var $author$project$Codegen$Utils$validChars = '[' + ($author$project$Codegen$Utils$validFirstUnicodeCharacters + ($author$project$Codegen$Utils$validRestUnicodeCharacters + ($author$project$Codegen$Utils$validUnicodeSymbols + ($author$project$Codegen$Utils$validUnicodeNumerals + ']'))));
var $author$project$Codegen$Utils$sanitizeRest = function (str) {
	return $elm$core$String$concat(
		A2(
			$elm$core$List$map,
			function ($) {
				return $.aS;
			},
			A2(
				$elm$regex$Regex$find,
				$author$project$Codegen$Utils$regex($author$project$Codegen$Utils$validChars),
				str)));
};
var $author$project$Codegen$Utils$keywords = _List_fromArray(
	['if', 'then', 'else', 'case', 'of', 'let', 'in', 'type', 'module', 'where', 'import', 'exposing', 'as', 'port', 'infix', 'infixl', 'infixr']);
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$Codegen$Utils$santizeKeywords = function (str) {
	return A2($elm$core$List$member, str, $author$project$Codegen$Utils$keywords) ? (str + '_') : str;
};
var $author$project$Codegen$Utils$sanitize = A2(
	$elm$core$Basics$composeR,
	$author$project$Codegen$Utils$sanitizeFirst,
	A2($elm$core$Basics$composeR, $author$project$Codegen$Utils$sanitizeRest, $author$project$Codegen$Utils$santizeKeywords));
var $author$project$Generate$Swagger$moduleName = A2($elm$core$Basics$composeL, $author$project$Codegen$Utils$capitalize, $author$project$Codegen$Utils$sanitize);
var $author$project$Generate$Utils$typeName = A2($elm$core$Basics$composeL, $author$project$Codegen$Utils$capitalize, $author$project$Codegen$Utils$sanitize);
var $author$project$Generate$Utils$decoderName = function (name) {
	return 'decode' + $author$project$Generate$Utils$typeName(name);
};
var $author$project$Codegen$Function$argName = function (_v0) {
	var type_ = _v0.a;
	var name = _v0.b;
	return name;
};
var $author$project$Codegen$Function$functionDeclaration = F4(
	function (name, args, type_, body) {
		return name + (' ' + (A2(
			$elm$core$String$join,
			' ',
			A2($elm$core$List$map, $author$project$Codegen$Function$argName, args)) + (' = \n' + body)));
	});
var $author$project$Codegen$Function$argType = function (_v0) {
	var type_ = _v0.a;
	var name = _v0.b;
	return type_;
};
var $author$project$Utils$flip = F3(
	function (fn, b, a) {
		return A2(fn, a, b);
	});
var $author$project$Codegen$Function$functionType = F4(
	function (name, args, type_, body) {
		return name + (' : ' + ($elm$core$String$concat(
			A2(
				$elm$core$List$map,
				A2(
					$elm$core$Basics$composeL,
					A2($author$project$Utils$flip, $elm$core$Basics$append, ' -> '),
					$author$project$Codegen$Function$argType),
				args)) + type_));
	});
var $author$project$Codegen$Function$function = F4(
	function (name, args, type_, body) {
		return A4($author$project$Codegen$Function$functionType, name, args, type_, body) + ('\n' + (A4($author$project$Codegen$Function$functionDeclaration, name, args, type_, body) + '\n'));
	});
var $author$project$Swagger$Definition$getFullName = function (def) {
	if (!def.$) {
		var name = def.a;
		return name;
	} else {
		var parentNames = def.a;
		var name = def.b;
		return A3(
			$elm$core$Basics$apL,
			$author$project$Utils$flip($elm$core$Basics$append),
			$author$project$Codegen$Utils$capitalize(name),
			$elm$core$String$concat(
				A2(
					$elm$core$List$map,
					$author$project$Codegen$Utils$capitalize,
					$elm$core$List$reverse(parentNames))));
	}
};
var $author$project$Swagger$Type$getItemsType = function (_v0) {
	var type_ = _v0;
	return type_;
};
var $author$project$Codegen$Function$pipeline = F2(
	function (init, items) {
		return _Utils_ap(
			init,
			$elm$core$String$concat(
				A2(
					$elm$core$List$map,
					$elm$core$Basics$append('\n  |> '),
					items)));
	});
var $author$project$Codegen$Function$lazy = function (body) {
	return '(lazy (\\_ -> ' + (body + '))');
};
var $author$project$Generate$Utils$nestedDecoderName = F2(
	function (parentName, name) {
		return 'decode' + ($author$project$Generate$Utils$typeName(parentName) + $author$project$Generate$Utils$typeName(name));
	});
var $author$project$Generate$Decoder$renderPropertyDecoder = F3(
	function (parentName, name, type_) {
		switch (type_.$) {
			case 0:
				return $author$project$Codegen$Function$lazy(
					A2($author$project$Generate$Utils$nestedDecoderName, parentName, name));
			case 1:
				return A2($author$project$Generate$Utils$nestedDecoderName, parentName, name);
			case 2:
				return A2($author$project$Generate$Utils$nestedDecoderName, parentName, name);
			case 4:
				return A2($author$project$Generate$Utils$nestedDecoderName, parentName, name);
			case 3:
				return 'string';
			case 5:
				return 'int';
			case 6:
				return 'float';
			case 7:
				return 'bool';
			default:
				var ref = type_.a;
				return $author$project$Codegen$Function$lazy(
					$author$project$Generate$Utils$decoderName(ref));
		}
	});
var $author$project$Generate$Decoder$renderArrayBody = F2(
	function (name, type_) {
		return A3(
			$author$project$Utils$flip,
			$author$project$Codegen$Function$pipeline,
			_List_fromArray(
				[
					'map ' + $author$project$Generate$Utils$typeName(name)
				]),
			'list ' + A3($author$project$Generate$Decoder$renderPropertyDecoder, name, 'Item', type_));
	});
var $author$project$Generate$Decoder$renderDictBody = F2(
	function (name, type_) {
		return 'dict ' + A3($author$project$Generate$Decoder$renderPropertyDecoder, name, 'Property', type_);
	});
var $author$project$Codegen$Function$caseof = F2(
	function (case_, conditions) {
		var conds = A2(
			$elm$core$String$join,
			'\n',
			A2(
				$elm$core$List$map,
				function (_v0) {
					var name = _v0.a;
					var body = _v0.b;
					return '      ' + (name + (' -> ' + body));
				},
				conditions));
		return '  case ' + (case_ + (' of\n' + conds));
	});
var $author$project$Codegen$Function$letin = F2(
	function (declarations, body) {
		var lets = A2(
			$elm$core$String$join,
			'\n',
			A2(
				$elm$core$List$map,
				function (_v0) {
					var name = _v0.a;
					var expr = _v0.b;
					return '      ' + (name + (' = ' + expr));
				},
				declarations));
		return '  let\n' + (lets + ('\n    in\n      ' + body));
	});
var $author$project$Generate$Utils$enumValueTypeName = F2(
	function (name, value) {
		if (value === '') {
			return name + '_None';
		} else {
			var s = value;
			return name + ('_' + $author$project$Generate$Utils$typeName(s));
		}
	});
var $author$project$Codegen$Literal$string = function (str) {
	return '\"' + (str + '\"');
};
var $author$project$Generate$Decoder$renderEnumEach = F2(
	function (name, value) {
		return _Utils_Tuple2(
			$author$project$Codegen$Literal$string(value),
			'Result.Ok ' + A2($author$project$Generate$Utils$enumValueTypeName, name, value));
	});
var $author$project$Generate$Decoder$renderEnumFail = function (parentName) {
	return _Utils_Tuple2(
		'_',
		'Result.Err (\"Invalid value for ' + ($author$project$Generate$Utils$typeName(parentName) + '. Value: \" ++ string)'));
};
var $author$project$Generate$Decoder$renderEnumBody = F2(
	function (parentName, _enum) {
		var decoderName_ = $author$project$Generate$Utils$decoderName(parentName);
		return A2(
			$author$project$Codegen$Function$letin,
			_List_fromArray(
				[
					_Utils_Tuple2(
					'decodeToType string',
					A2(
						$author$project$Codegen$Function$caseof,
						'string',
						_Utils_ap(
							A2(
								$elm$core$List$map,
								$author$project$Generate$Decoder$renderEnumEach(parentName),
								_enum),
							_List_fromArray(
								[
									$author$project$Generate$Decoder$renderEnumFail(parentName)
								]))))
				]),
			'customDecoder string decodeToType');
	});
var $author$project$Generate$Decoder$defaultValue = F2(
	function (type_, _default) {
		if (type_.$ === 4) {
			var _v1 = A2($elm$json$Json$Decode$decodeString, $elm$json$Json$Decode$string, _default);
			if (!_v1.$) {
				var newDefault = _v1.a;
				return $author$project$Generate$Utils$typeName(newDefault);
			} else {
				var err = _v1.a;
				return 'Invalid default value:' + $elm$json$Json$Decode$errorToString(err);
			}
		} else {
			return _default;
		}
	});
var $author$project$Generate$Decoder$renderObjectDecoderProperty = F2(
	function (parentName, property) {
		switch (property.$) {
			case 0:
				var name = property.a;
				var type_ = property.b;
				return 'required ' + ($author$project$Codegen$Literal$string(name) + (' ' + A3($author$project$Generate$Decoder$renderPropertyDecoder, parentName, name, type_)));
			case 1:
				var name = property.a;
				var type_ = property.b;
				return 'maybe ' + ($author$project$Codegen$Literal$string(name) + (' ' + A3($author$project$Generate$Decoder$renderPropertyDecoder, parentName, name, type_)));
			default:
				var name = property.a;
				var type_ = property.b;
				var _default = property.c;
				return 'optional ' + ($author$project$Codegen$Literal$string(name) + (' ' + (A3($author$project$Generate$Decoder$renderPropertyDecoder, parentName, name, type_) + (' ' + A2($author$project$Generate$Decoder$defaultValue, type_, _default)))));
		}
	});
var $author$project$Generate$Decoder$renderObjectBody = F2(
	function (name, _v0) {
		var properties = _v0;
		return A2(
			$author$project$Codegen$Function$pipeline,
			'Json.Decode.succeed ' + $author$project$Generate$Utils$typeName(name + 'Record'),
			A3(
				$author$project$Utils$flip,
				$elm$core$Basics$append,
				_List_fromArray(
					[
						'map ' + $author$project$Generate$Utils$typeName(name)
					]),
				A2(
					$elm$core$List$map,
					$author$project$Generate$Decoder$renderObjectDecoderProperty(name),
					properties)));
	});
var $author$project$Generate$Decoder$renderPrimitiveBody = function (type_) {
	return type_;
};
var $author$project$Generate$Decoder$renderDecoderBody = function (definition) {
	var _v0 = $author$project$Swagger$Definition$getType(definition);
	switch (_v0.$) {
		case 0:
			var properties = _v0.a;
			return A2(
				$author$project$Generate$Decoder$renderObjectBody,
				$author$project$Swagger$Definition$getFullName(definition),
				properties);
		case 1:
			var items = _v0.a;
			return A2(
				$author$project$Generate$Decoder$renderArrayBody,
				$author$project$Swagger$Definition$getFullName(definition),
				$author$project$Swagger$Type$getItemsType(items));
		case 2:
			var typeName = _v0.a;
			return A2(
				$author$project$Generate$Decoder$renderDictBody,
				$author$project$Swagger$Definition$getFullName(definition),
				typeName);
		case 4:
			var _enum = _v0.b;
			return A2(
				$author$project$Generate$Decoder$renderEnumBody,
				$author$project$Swagger$Definition$getFullName(definition),
				_enum);
		case 3:
			return $author$project$Generate$Decoder$renderPrimitiveBody('string');
		case 5:
			return $author$project$Generate$Decoder$renderPrimitiveBody('int');
		case 6:
			return $author$project$Generate$Decoder$renderPrimitiveBody('float');
		case 7:
			return $author$project$Generate$Decoder$renderPrimitiveBody('bool');
		default:
			var ref = _v0.a;
			return $author$project$Generate$Utils$decoderName(ref);
	}
};
var $author$project$Generate$Decoder$renderDecoder = function (definition) {
	var name = $author$project$Swagger$Definition$getFullName(definition);
	return A4(
		$author$project$Codegen$Function$function,
		$author$project$Generate$Utils$decoderName(name),
		_List_Nil,
		'Decoder ' + $author$project$Generate$Utils$typeName(name),
		$author$project$Generate$Decoder$renderDecoderBody(definition));
};
var $author$project$Codegen$Function$Arg = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Codegen$Function$arg = F2(
	function (type_, name) {
		return A2($author$project$Codegen$Function$Arg, type_, name);
	});
var $author$project$Generate$Utils$encoderName = function (name) {
	return 'encode' + $author$project$Generate$Utils$typeName(name);
};
var $author$project$Generate$Encoder$maybeUnwrapType = F2(
	function (definition, name) {
		var _v0 = $author$project$Swagger$Definition$getType(definition);
		switch (_v0.$) {
			case 0:
				return '(' + ($author$project$Generate$Utils$typeName(name) + (' value' + ')'));
			case 1:
				return '(' + ($author$project$Generate$Utils$typeName(name) + (' value' + ')'));
			case 8:
				var ref = _v0.a;
				return '(' + ($author$project$Generate$Utils$typeName(name) + (' value' + ')'));
			case 2:
				return 'value';
			case 4:
				var _enum = _v0.b;
				return 'value';
			case 3:
				return 'value';
			case 5:
				return 'value';
			case 6:
				return 'value';
			default:
				return 'value';
		}
	});
var $author$project$Generate$Utils$nestedEncoderName = F2(
	function (parentName, name) {
		return 'encode' + ($author$project$Generate$Utils$typeName(parentName) + $author$project$Generate$Utils$typeName(name));
	});
var $author$project$Generate$Encoder$renderPropertyEncoder = F3(
	function (parentName, name, type_) {
		switch (type_.$) {
			case 0:
				return A2($author$project$Generate$Utils$nestedEncoderName, parentName, name);
			case 1:
				return A2($author$project$Generate$Utils$nestedEncoderName, parentName, name);
			case 2:
				return A2($author$project$Generate$Utils$nestedEncoderName, parentName, name);
			case 4:
				return A2($author$project$Generate$Utils$nestedEncoderName, parentName, name);
			case 3:
				return 'Json.Encode.string';
			case 5:
				return 'Json.Encode.int';
			case 6:
				return 'Json.Encode.float';
			case 7:
				return 'Json.Encode.bool';
			default:
				var ref = type_.a;
				return $author$project$Generate$Utils$encoderName(ref);
		}
	});
var $author$project$Generate$Encoder$renderArrayBody = F2(
	function (name, type_) {
		return 'Json.Encode.list ' + (A3($author$project$Generate$Encoder$renderPropertyEncoder, name, 'Item', type_) + ' value');
	});
var $author$project$Generate$Encoder$renderDictBody = F2(
	function (name, typeName) {
		return 'dictEncoder ' + (A3($author$project$Generate$Encoder$renderPropertyEncoder, name, 'Property', typeName) + ' value');
	});
var $author$project$Generate$Encoder$renderEnumEach = F2(
	function (name, value) {
		return _Utils_Tuple2(
			A2($author$project$Generate$Utils$enumValueTypeName, name, value),
			'Json.Encode.string ' + $author$project$Codegen$Literal$string(value));
	});
var $author$project$Generate$Encoder$renderEnumBody = F2(
	function (parentName, _enum) {
		return A2(
			$author$project$Codegen$Function$caseof,
			'value',
			A2(
				$elm$core$List$map,
				$author$project$Generate$Encoder$renderEnumEach(parentName),
				_enum));
	});
var $author$project$Codegen$List$list = A2(
	$elm$core$Basics$composeR,
	$elm$core$String$join('\n  , '),
	A2(
		$elm$core$Basics$composeR,
		$elm$core$Basics$append('[ '),
		A2($author$project$Utils$flip, $elm$core$Basics$append, ' ]')));
var $author$project$Codegen$Tuple$tuple = F2(
	function (fst, snd) {
		return '( ' + (fst + (', ' + (snd + ')')));
	});
var $elm$core$String$toLower = _String_toLower;
var $author$project$Codegen$Utils$uncapitalize = function (str) {
	var _v0 = $elm$core$String$uncons(str);
	if (!_v0.$) {
		var _v1 = _v0.a;
		var head = _v1.a;
		var tail = _v1.b;
		return _Utils_ap(
			$elm$core$String$toLower(
				$elm$core$String$fromChar(head)),
			tail);
	} else {
		return '';
	}
};
var $author$project$Generate$Encoder$renderObjectProperty = F2(
	function (parentName, property) {
		var propertyEncoder = A3(
			$author$project$Generate$Encoder$renderPropertyEncoder,
			parentName,
			$author$project$Swagger$Type$getPropertyName(property),
			$author$project$Swagger$Type$getPropertyType(property));
		switch (property.$) {
			case 0:
				var name = property.a;
				var type_ = property.b;
				return 'Just ' + A2(
					$author$project$Codegen$Tuple$tuple,
					$author$project$Codegen$Literal$string(name),
					propertyEncoder + (' value.' + $author$project$Codegen$Utils$uncapitalize(
						$author$project$Codegen$Utils$sanitize(name))));
			case 1:
				var name = property.a;
				var type_ = property.b;
				return 'Maybe.map (\\v -> ' + (A2(
					$author$project$Codegen$Tuple$tuple,
					$author$project$Codegen$Literal$string(name),
					propertyEncoder + ' v') + (') value.' + $author$project$Codegen$Utils$uncapitalize(
					$author$project$Codegen$Utils$sanitize(name))));
			default:
				var name = property.a;
				var type_ = property.b;
				return 'Just ' + A2(
					$author$project$Codegen$Tuple$tuple,
					$author$project$Codegen$Literal$string(name),
					propertyEncoder + (' value.' + $author$project$Codegen$Utils$uncapitalize(
						$author$project$Codegen$Utils$sanitize(name))));
		}
	});
var $author$project$Generate$Encoder$renderObjectBody = F2(
	function (name, _v0) {
		var properties = _v0;
		return 'Json.Encode.object <| List.filterMap identity ' + $author$project$Codegen$List$list(
			A2(
				$elm$core$List$map,
				$author$project$Generate$Encoder$renderObjectProperty(name),
				properties));
	});
var $author$project$Generate$Encoder$renderPrimitiveBody = function (typeName) {
	return 'Json.Encode.' + (typeName + ' value');
};
var $author$project$Generate$Encoder$renderEncoderBody = function (definition) {
	var _v0 = $author$project$Swagger$Definition$getType(definition);
	switch (_v0.$) {
		case 0:
			var properties = _v0.a;
			return A2(
				$author$project$Generate$Encoder$renderObjectBody,
				$author$project$Swagger$Definition$getFullName(definition),
				properties);
		case 1:
			var items = _v0.a;
			return A2(
				$author$project$Generate$Encoder$renderArrayBody,
				$author$project$Swagger$Definition$getFullName(definition),
				$author$project$Swagger$Type$getItemsType(items));
		case 2:
			var typeName = _v0.a;
			return A2(
				$author$project$Generate$Encoder$renderDictBody,
				$author$project$Swagger$Definition$getFullName(definition),
				typeName);
		case 4:
			var _enum = _v0.b;
			return A2(
				$author$project$Generate$Encoder$renderEnumBody,
				$author$project$Swagger$Definition$getFullName(definition),
				_enum);
		case 3:
			return $author$project$Generate$Encoder$renderPrimitiveBody('string');
		case 5:
			return $author$project$Generate$Encoder$renderPrimitiveBody('int');
		case 6:
			return $author$project$Generate$Encoder$renderPrimitiveBody('float');
		case 7:
			return $author$project$Generate$Encoder$renderPrimitiveBody('bool');
		default:
			var ref = _v0.a;
			return $author$project$Generate$Utils$encoderName(ref);
	}
};
var $author$project$Generate$Encoder$renderEncoder = function (definition) {
	var name = $author$project$Swagger$Definition$getFullName(definition);
	return A4(
		$author$project$Codegen$Function$function,
		$author$project$Generate$Utils$encoderName(name),
		_List_fromArray(
			[
				A2(
				$author$project$Codegen$Function$arg,
				$author$project$Generate$Utils$typeName(name),
				A2($author$project$Generate$Encoder$maybeUnwrapType, definition, name))
			]),
		'Json.Encode.Value',
		$author$project$Generate$Encoder$renderEncoderBody(definition));
};
var $author$project$Codegen$Type$dict = function (typeName) {
	return 'Dict String ' + typeName;
};
var $author$project$Codegen$Type$wrap = F2(
	function (name, body) {
		return name + (' (' + (body + ')'));
	});
var $author$project$Codegen$Type$list = function (body) {
	return A2($author$project$Codegen$Type$wrap, 'List', body);
};
var $author$project$Generate$Type$renderEnum = function (name) {
	return $elm$core$List$map(
		$author$project$Generate$Utils$enumValueTypeName(name));
};
var $author$project$Generate$Utils$nestedTypeName = F2(
	function (parentName, name) {
		return _Utils_ap(
			$author$project$Generate$Utils$typeName(parentName),
			$author$project$Generate$Utils$typeName(name));
	});
var $author$project$Generate$Type$renderPropertyType = F3(
	function (parentName, name, type_) {
		switch (type_.$) {
			case 0:
				return A2($author$project$Generate$Utils$nestedTypeName, parentName, name);
			case 1:
				return A2($author$project$Generate$Utils$nestedTypeName, parentName, name);
			case 2:
				return A2($author$project$Generate$Utils$nestedTypeName, parentName, name);
			case 4:
				return A2($author$project$Generate$Utils$nestedTypeName, parentName, name);
			case 3:
				return 'String';
			case 5:
				return 'Int';
			case 6:
				return 'Float';
			case 7:
				return 'Bool';
			default:
				var ref = type_.a;
				return $author$project$Generate$Utils$typeName(ref);
		}
	});
var $author$project$Codegen$Type$record = function (properties) {
	return '\n  { ' + (A2($elm$core$String$join, '\n  , ', properties) + '\n  }\n');
};
var $author$project$Codegen$Type$maybe = function (body) {
	return A2($author$project$Codegen$Type$wrap, 'Maybe', body);
};
var $author$project$Codegen$Type$recordField = F2(
	function (name, type_) {
		return $author$project$Codegen$Utils$uncapitalize(name) + (' : ' + type_);
	});
var $author$project$Generate$Type$renderProperty = F2(
	function (parentName, prop) {
		switch (prop.$) {
			case 0:
				var name = prop.a;
				var type_ = prop.b;
				return A2(
					$author$project$Codegen$Type$recordField,
					$author$project$Codegen$Utils$sanitize(name),
					A3($author$project$Generate$Type$renderPropertyType, parentName, name, type_));
			case 1:
				var name = prop.a;
				var type_ = prop.b;
				return A2(
					$author$project$Codegen$Type$recordField,
					$author$project$Codegen$Utils$sanitize(name),
					$author$project$Codegen$Type$maybe(
						A3($author$project$Generate$Type$renderPropertyType, parentName, name, type_)));
			default:
				var name = prop.a;
				var type_ = prop.b;
				return A2(
					$author$project$Codegen$Type$recordField,
					$author$project$Codegen$Utils$sanitize(name),
					A3($author$project$Generate$Type$renderPropertyType, parentName, name, type_));
		}
	});
var $author$project$Generate$Type$renderRecord = F2(
	function (parentName, _v0) {
		var properties = _v0;
		return $author$project$Codegen$Type$record(
			A2(
				$elm$core$List$map,
				$author$project$Generate$Type$renderProperty(parentName),
				properties));
	});
var $author$project$Codegen$Type$typeAlias = F2(
	function (name, body) {
		return 'type alias ' + ($author$project$Codegen$Utils$capitalize(name) + (' = ' + (body + '\n')));
	});
var $author$project$Codegen$Type$unionType = F2(
	function (name, tags) {
		return 'type ' + ($author$project$Codegen$Utils$capitalize(name) + ('\n  = ' + (A2($elm$core$String$join, '\n  | ', tags) + '\n')));
	});
var $author$project$Generate$Type$renderType = function (definition) {
	var type_ = $author$project$Swagger$Definition$getType(definition);
	var name = $author$project$Generate$Utils$typeName(
		$author$project$Swagger$Definition$getFullName(definition));
	var objectDecl = $author$project$Codegen$Type$typeAlias(name + 'Record');
	var recordDecl = 'type ' + (name + (' = ' + (name + (' ' + (name + 'Record \n\n')))));
	var typeAliasDecl = $author$project$Codegen$Type$typeAlias(name);
	var unionTypeDecl = $author$project$Codegen$Type$unionType(name);
	var arrayDecl = function (body) {
		return 'type ' + (name + (' = ' + (name + (' (' + (body + ')\n\n')))));
	};
	switch (type_.$) {
		case 3:
			return typeAliasDecl('String');
		case 5:
			return typeAliasDecl('Int');
		case 6:
			return typeAliasDecl('Float');
		case 7:
			return typeAliasDecl('Bool');
		case 4:
			var _enum = type_.b;
			return unionTypeDecl(
				A2($author$project$Generate$Type$renderEnum, name, _enum));
		case 0:
			var props = type_.a;
			return _Utils_ap(
				objectDecl(
					A2($author$project$Generate$Type$renderRecord, name, props)),
				recordDecl);
		case 1:
			var items = type_.a;
			return arrayDecl(
				$author$project$Codegen$Type$list(
					A3(
						$author$project$Generate$Type$renderPropertyType,
						name,
						'Item',
						$author$project$Swagger$Type$getItemsType(items))));
		case 2:
			var typename = type_.a;
			return typeAliasDecl(
				$author$project$Codegen$Type$dict(
					A3($author$project$Generate$Type$renderPropertyType, name, 'Property', typename)));
		default:
			var ref = type_.a;
			return typeAliasDecl(
				$author$project$Generate$Utils$typeName(ref));
	}
};
var $author$project$Generate$Swagger$renderDefinition = function (definition) {
	var name_ = $author$project$Generate$Swagger$moduleName(
		$author$project$Swagger$Definition$getName(definition));
	return $elm$core$String$concat(
		_List_fromArray(
			[
				$author$project$Generate$Type$renderType(definition),
				$author$project$Generate$Decoder$renderDecoder(definition),
				$author$project$Generate$Encoder$renderEncoder(definition),
				'\n\n'
			]));
};
var $author$project$Generate$Headers$renderHeaders = 'module Swagger exposing (..)\n\nimport Json.Decode exposing (Decoder, string, int, float, dict, list, bool, map, value, decodeValue, decodeString, lazy, succeed, fail, andThen)\nimport Json.Decode.Pipeline exposing (required, optional, hardcoded)\nimport Json.Encode\nimport Json.Encode.Extra\nimport Dict exposing (Dict)\n\n\nmaybe : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b\nmaybe name decoder =\n    optional name (map Just decoder) Nothing\n\n\ncustomDecoder : Decoder a -> (a -> Result String b) -> Decoder b\ncustomDecoder decoder toResult =\n    andThen\n        (\\a ->\n            case toResult a of\n                Ok b ->\n                    succeed b\n\n                Err err ->\n                    fail err\n        )\n        decoder\n\n\ndictEncoder : (a -> Json.Encode.Value) -> Dict String a -> Json.Encode.Value\ndictEncoder enc dict =\n    Dict.toList dict\n        |> List.map (\\(k,v) -> (k, enc v))\n        |> Json.Encode.object\n\n';
var $author$project$Generate$Swagger$render = function (_v0) {
	var definitions = _v0.aJ;
	return _Utils_ap(
		$author$project$Generate$Headers$renderHeaders,
		$elm$core$String$concat(
			A2($author$project$Swagger$Definition$map, $author$project$Generate$Swagger$renderDefinition, definitions)));
};
var $author$project$Generate$generate = function (json) {
	return A2(
		$elm$core$Result$map,
		A2($elm$core$Basics$composeR, $author$project$Swagger$Flatten$flatten, $author$project$Generate$Swagger$render),
		A2(
			$elm$core$Result$mapError,
			$elm$json$Json$Decode$errorToString,
			A2($elm$json$Json$Decode$decodeString, $author$project$Swagger$Decode$decodeSwagger, json)));
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Ports$printAndExitFailure = _Platform_outgoingPort('printAndExitFailure', $elm$json$Json$Encode$string);
var $author$project$Ports$printAndExitSuccess = _Platform_outgoingPort('printAndExitSuccess', $elm$json$Json$Encode$string);
var $author$project$Generate$init = function (json) {
	return _Utils_Tuple2(
		0,
		function () {
			var _v0 = $author$project$Generate$generate(json);
			if (!_v0.$) {
				var result = _v0.a;
				return $author$project$Ports$printAndExitSuccess(result);
			} else {
				var err = _v0.a;
				return $author$project$Ports$printAndExitFailure(err);
			}
		}());
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$core$Platform$worker = _Platform_worker;
var $author$project$Generate$main = $elm$core$Platform$worker(
	{
		aQ: $author$project$Generate$init,
		a$: $elm$core$Basics$always($elm$core$Platform$Sub$none),
		a1: F2(
			function (_v0, _v1) {
				return _Utils_Tuple2(0, $elm$core$Platform$Cmd$none);
			})
	});
_Platform_export({'Generate':{'init':$author$project$Generate$main($elm$json$Json$Decode$string)(0)}});}(this));