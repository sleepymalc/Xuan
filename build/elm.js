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

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


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

	/**/
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

	/**_UNUSED/
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

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
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

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


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



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


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

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
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

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
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


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
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
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
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



/**/
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

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

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
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
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


function _Platform_export_UNUSED(exports)
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


function _Platform_export(exports)
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




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
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
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
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
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
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
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
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
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
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
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
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
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
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
	return {$: 'SubTree', a: a};
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
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
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
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
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
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
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
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Message$GetViewport = function (a) {
	return {$: 'GetViewport', a: a};
};
var $author$project$Model$LOGO = {$: 'LOGO'};
var $author$project$Model$Vector = F2(
	function (x, y) {
		return {x: x, y: y};
	});
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$browser$Browser$Dom$getViewport = _Browser_withWindow(_Browser_getViewport);
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)),
			string);
	});
var $author$project$Load$connectName = F3(
	function (namePrefix, anim, id) {
		return namePrefix + (anim + ('/' + (namePrefix + (anim + ('_' + A3(
			$elm$core$String$padLeft,
			4,
			_Utils_chr('0'),
			$elm$core$String$fromInt(id)))))));
	});
var $author$project$Load$initLoadPack = function () {
	var surfix = '.png';
	var prefix = 'http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/character/color/';
	var names = _Utils_ap(
		A2(
			$elm$core$List$map,
			A2($author$project$Load$connectName, '', 'walk'),
			A2($elm$core$List$range, 0, 64)),
		_Utils_ap(
			A2(
				$elm$core$List$map,
				A2($author$project$Load$connectName, 'character_', 'walk'),
				A2($elm$core$List$range, 0, 59)),
			_Utils_ap(
				A2(
					$elm$core$List$map,
					A2($author$project$Load$connectName, '', 'attack'),
					A2($elm$core$List$range, 0, 59)),
				_Utils_ap(
					A2(
						$elm$core$List$map,
						A2($author$project$Load$connectName, 'character_', 'attack'),
						A2($elm$core$List$range, 0, 59)),
					_Utils_ap(
						A2(
							$elm$core$List$map,
							A2($author$project$Load$connectName, '', 'charge'),
							A2($elm$core$List$range, 0, 2)),
						_List_fromArray(
							[
								A3($author$project$Load$connectName, '', 'jump', 0),
								'attacked/attackedBack_0000',
								'attacked/attackedFront_0000'
							]))))));
	var urls = _Utils_ap(
		A2(
			$elm$core$List$map,
			function (name) {
				return _Utils_ap(
					prefix,
					_Utils_ap(name, surfix));
			},
			names),
		_List_fromArray(
			['http://focs.ji.sjtu.edu.cn/vg100/demo/p2team13/img/background1.png']));
	return urls;
}();
var $author$project$MapSetting$Pos = F4(
	function (x1, x2, y1, y2) {
		return {x1: x1, x2: x2, y1: y1, y2: y2};
	});
var $author$project$MapSetting$birdPosList1 = _List_fromArray(
	[
		A4($author$project$MapSetting$Pos, 300, 400, 3830, 3930)
	]);
var $author$project$MapSetting$brickPosList1 = _List_fromArray(
	[
		A4($author$project$MapSetting$Pos, 3200, 10000, 3900, 4000),
		A4($author$project$MapSetting$Pos, 2900, 3200, 3800, 3935),
		A4($author$project$MapSetting$Pos, 2350, 2650, 3635, 3700),
		A4($author$project$MapSetting$Pos, 1800, 2100, 3470, 3535),
		A4($author$project$MapSetting$Pos, 2800, 3100, 3300, 3365),
		A4($author$project$MapSetting$Pos, 2250, 2500, 3175, 3240),
		A4($author$project$MapSetting$Pos, 1700, 2000, 3035, 3100),
		A4($author$project$MapSetting$Pos, 2700, 3000, 2900, 3000),
		A4($author$project$MapSetting$Pos, 2175, 2475, 2700, 2800),
		A4($author$project$MapSetting$Pos, 1700, 2000, 2350, 2485),
		A4($author$project$MapSetting$Pos, 2250, 2450, 2150, 2250),
		A4($author$project$MapSetting$Pos, 2600, 3050, 1950, 2050),
		A4($author$project$MapSetting$Pos, 2000, 2500, 1450, 1700),
		A4($author$project$MapSetting$Pos, 2500, 2750, 1750, 1800),
		A4($author$project$MapSetting$Pos, 1900, 2300, 1160, 1215),
		A4($author$project$MapSetting$Pos, 2500, 2900, 1160, 1215),
		A4($author$project$MapSetting$Pos, 1900, 2300, 790, 930),
		A4($author$project$MapSetting$Pos, 2500, 2900, 790, 930)
	]);
var $author$project$MapSetting$brickWallList1 = _List_fromArray(
	[
		A4($author$project$MapSetting$Pos, 0, 1, 3200, 3935),
		A4($author$project$MapSetting$Pos, 0, 1500, 3100, 3200),
		A4($author$project$MapSetting$Pos, 1700, 3200, 0, 100),
		A4($author$project$MapSetting$Pos, 0, 3135, 3935, 4000),
		A4($author$project$MapSetting$Pos, 1600, 1700, 0, 3200),
		A4($author$project$MapSetting$Pos, 3170, 3200, 775, 4000),
		A4($author$project$MapSetting$Pos, 3200, 3900, 750, 875)
	]);
var $author$project$MapSetting$characterPosList1 = _List_fromArray(
	[
		A4($author$project$MapSetting$Pos, 2900, 3000, 3205, 3305),
		A4($author$project$MapSetting$Pos, 1800, 1900, 2935, 3035),
		A4($author$project$MapSetting$Pos, 1800, 1900, 2250, 2350),
		A4($author$project$MapSetting$Pos, 2000, 2100, 1075, 1175),
		A4($author$project$MapSetting$Pos, 2700, 2800, 1075, 1175),
		A4($author$project$MapSetting$Pos, 2430, 2530, 3540, 3640)
	]);
var $author$project$MapSetting$exitPos1 = A4($author$project$MapSetting$Pos, 3200, 10000, 2000, 3900);
var $author$project$Message$Right = {$: 'Right'};
var $author$project$Model$Stand = {$: 'Stand'};
var $author$project$Model$initBirds1 = function (posList) {
	return A2(
		$elm$core$List$map,
		function (pos) {
			return {
				anim: $author$project$Model$Stand,
				direction: $author$project$Message$Right,
				frame: 0,
				pos: pos,
				speed: A2($author$project$Model$Vector, 0, 0)
			};
		},
		posList);
};
var $author$project$Model$initBricks = function (posList) {
	return A2(
		$elm$core$List$map,
		function (pos) {
			return {
				pos: pos,
				speed: A2($author$project$Model$Vector, 0, 0)
			};
		},
		posList);
};
var $author$project$Message$Left = {$: 'Left'};
var $author$project$Message$Up = {$: 'Up'};
var $author$project$Model$Walk = {$: 'Walk'};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $author$project$Model$initCharacters = function (posList) {
	return A2(
		$elm$core$List$map,
		function (pos) {
			return {
				anim: $author$project$Model$Walk,
				chargetime: 0,
				collisionPos: _List_fromArray(
					[pos]),
				direction: $author$project$Message$Left,
				effecttime: 0,
				frame: 0,
				hp: 1,
				jumpdir: $author$project$Message$Up,
				pos: pos,
				range: A2($author$project$Model$Vector, pos.x1 - 100, pos.x2 + 100),
				speed: A2($author$project$Model$Vector, -0.05, 0)
			};
		},
		posList);
};
var $author$project$Model$initNpcs1 = function (posList) {
	return A2(
		$elm$core$List$map,
		function (pos) {
			return {
				anim: $author$project$Model$Stand,
				count: 0,
				direction: $author$project$Message$Left,
				frame: 0,
				pos: pos,
				speed: A2($author$project$Model$Vector, 0, 0),
				text: 'Go UP!!! ESCAPE!!! By the way, nice to see you!',
				textframe: 0
			};
		},
		posList);
};
var $author$project$MapSetting$npcPosList1 = _List_fromArray(
	[
		A4($author$project$MapSetting$Pos, 3100, 3200, 3695, 3795)
	]);
var $author$project$Model$initMap1 = {
	birds: $author$project$Model$initBirds1($author$project$MapSetting$birdPosList1),
	bricks: $author$project$Model$initBricks($author$project$MapSetting$brickPosList1),
	characters: $author$project$Model$initCharacters($author$project$MapSetting$characterPosList1),
	exit: $author$project$MapSetting$exitPos1,
	npcs: $author$project$Model$initNpcs1($author$project$MapSetting$npcPosList1),
	wallbricks: $author$project$Model$initBricks($author$project$MapSetting$brickWallList1)
};
var $author$project$Model$Crouch = {$: 'Crouch'};
var $author$project$Model$Normal = {$: 'Normal'};
var $author$project$MapSetting$playerPos1 = A4($author$project$MapSetting$Pos, 300, 400, 3830, 3930);
var $author$project$Model$standcollisionPos = function (pos) {
	return _List_fromArray(
		[
			A4($author$project$MapSetting$Pos, pos.x1 + 35, pos.x2 - 35, pos.y1 + 4, pos.y1 + 35),
			A4($author$project$MapSetting$Pos, pos.x1 + 15, pos.x2 - 15, pos.y1 + 35, pos.y2)
		]);
};
var $author$project$Model$initPlayer1 = {
	anim: $author$project$Model$Crouch,
	chargetime: 0,
	collisionPos: $author$project$Model$standcollisionPos($author$project$MapSetting$playerPos1),
	direction: $author$project$Message$Right,
	effecttimeFive: 800,
	effecttimeFour: 600,
	effecttimeOne: 0,
	effecttimeThree: 400,
	effecttimeTwo: 200,
	fallcount: 0,
	frame: 0,
	hp: 10,
	inrage: false,
	jumpdir: $author$project$Message$Up,
	mood: $author$project$Model$Normal,
	pos: $author$project$MapSetting$playerPos1,
	ragecount: 0,
	ragetime: 0,
	speed: A2($author$project$Model$Vector, 0, 0),
	teachtextstate: 0,
	text: 'I need to get outta here.',
	textframe: -1000
};
var $author$project$AISettings$AICharge = F2(
	function (a, b) {
		return {$: 'AICharge', a: a, b: b};
	});
var $author$project$AISettings$AIWalk = F2(
	function (a, b) {
		return {$: 'AIWalk', a: a, b: b};
	});
var $author$project$Message$L = {$: 'L'};
var $author$project$Message$R = {$: 'R'};
var $author$project$AISettings$initSpeedAIAnimList = _List_fromArray(
	[
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, true),
		time: 1343
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, false),
		time: 1598
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, true),
		time: 1700
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, false),
		time: 1768
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$Up, true),
		time: 1921
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$L, false),
		time: 2924
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, true),
		time: 3553
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, false),
		time: 4369
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$Up, true),
		time: 4505
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$L, false),
		time: 6715
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, true),
		time: 7803
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, false),
		time: 7888
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$Up, true),
		time: 8024
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$R, false),
		time: 9826
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, true),
		time: 10965
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, false),
		time: 11050
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$Up, true),
		time: 11220
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$L, false),
		time: 12699
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, true),
		time: 13889
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, false),
		time: 14654
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, true),
		time: 14790
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, false),
		time: 14875
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$Up, true),
		time: 14960
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$L, false),
		time: 17102
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, true),
		time: 18411
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, false),
		time: 18496
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$Up, true),
		time: 18632
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$R, false),
		time: 20808
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, true),
		time: 21539
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, false),
		time: 22746
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, true),
		time: 22848
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, false),
		time: 22950
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$Up, true),
		time: 22984
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$L, false),
		time: 25211
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, true),
		time: 26605
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, false),
		time: 26690
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$Up, true),
		time: 26775
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$L, false),
		time: 28339
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, true),
		time: 29529
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, false),
		time: 30345
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$Up, true),
		time: 30413
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$R, false),
		time: 32215
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, true),
		time: 33286
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, false),
		time: 35394
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, true),
		time: 35513
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, false),
		time: 35615
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$Up, true),
		time: 35785
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$L, false),
		time: 37145
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, true),
		time: 38131
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, false),
		time: 38233
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$Up, true),
		time: 38403
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$L, false),
		time: 40103
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, true),
		time: 41463
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, false),
		time: 41650
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, true),
		time: 41701
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, false),
		time: 41803
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$Up, true),
		time: 42007
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$L, false),
		time: 43843
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, true),
		time: 45084
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, false),
		time: 45186
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$Up, true),
		time: 45543
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$L, false),
		time: 47940
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, true),
		time: 49096
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, false),
		time: 50014
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, true),
		time: 50048
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, false),
		time: 50133
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$Up, true),
		time: 50320
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$L, false),
		time: 51816
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, true),
		time: 52632
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, false),
		time: 55250
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, true),
		time: 55454
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, false),
		time: 55539
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$Up, true),
		time: 55726
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$R, false),
		time: 57851
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, true),
		time: 58582
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Right, false),
		time: 60231
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, true),
		time: 60299
	},
		{
		msg: A2($author$project$AISettings$AIWalk, $author$project$Message$Left, false),
		time: 60418
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$Up, true),
		time: 60605
	},
		{
		msg: A2($author$project$AISettings$AICharge, $author$project$Message$L, false),
		time: 62118
	}
	]);
var $author$project$MapSetting$speedAIPos2 = A4($author$project$MapSetting$Pos, 1200, 1300, 4630, 4730);
var $author$project$Model$initSpeedAI = {
	anim: $author$project$Model$Stand,
	chargetime: 0,
	collisionPos: $author$project$Model$standcollisionPos($author$project$MapSetting$speedAIPos2),
	direction: $author$project$Message$Left,
	fallcount: 0,
	frame: 0,
	hp: 10,
	jumpdir: $author$project$Message$Up,
	pos: $author$project$MapSetting$speedAIPos2,
	speed: A2($author$project$Model$Vector, 0, 0),
	speedAIAnimList: $author$project$AISettings$initSpeedAIAnimList
};
var $author$project$Model$initstory = {storyframe: 0, text: ''};
var $author$project$Model$init = function (_v0) {
	return _Utils_Tuple2(
		{
			attrs: {},
			audioList: _List_Nil,
			cgtime: 5000,
			loadPack: $author$project$Load$initLoadPack,
			map: $author$project$Model$initMap1,
			player: $author$project$Model$initPlayer1,
			record: _List_Nil,
			size: A2($author$project$Model$Vector, 0, 0),
			speedAI: $author$project$Model$initSpeedAI,
			state: $author$project$Model$LOGO,
			story: $author$project$Model$initstory,
			time: 0
		},
		$elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					A2($elm$core$Task$perform, $author$project$Message$GetViewport, $elm$browser$Browser$Dom$getViewport)
				])));
};
var $author$project$Model$Rage = {$: 'Rage'};
var $author$project$Message$Resize = F2(
	function (a, b) {
		return {$: 'Resize', a: a, b: b};
	});
var $author$project$Message$Tick = function (a) {
	return {$: 'Tick', a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$html$Html$Events$keyCode = A2($elm$json$Json$Decode$field, 'keyCode', $elm$json$Json$Decode$int);
var $author$project$Message$AnimAttack = function (a) {
	return {$: 'AnimAttack', a: a};
};
var $author$project$Message$AnimCharge = function (a) {
	return {$: 'AnimCharge', a: a};
};
var $author$project$Message$AnimWalk = F2(
	function (a, b) {
		return {$: 'AnimWalk', a: a, b: b};
	});
var $author$project$Message$DebugDown = function (a) {
	return {$: 'DebugDown', a: a};
};
var $author$project$Message$DebugLeft = function (a) {
	return {$: 'DebugLeft', a: a};
};
var $author$project$Message$DebugRight = function (a) {
	return {$: 'DebugRight', a: a};
};
var $author$project$Message$DebugUp = function (a) {
	return {$: 'DebugUp', a: a};
};
var $author$project$Message$ExitDebugMode = {$: 'ExitDebugMode'};
var $author$project$Message$Noop = {$: 'Noop'};
var $author$project$Main$keyNormal = F2(
	function (on, keycode) {
		switch (keycode) {
			case 65:
				return A2($author$project$Message$AnimWalk, $author$project$Message$Left, on);
			case 68:
				return A2($author$project$Message$AnimWalk, $author$project$Message$Right, on);
			case 32:
				return $author$project$Message$AnimCharge(on);
			case 66:
				return $author$project$Message$AnimCharge(on);
			case 74:
				return $author$project$Message$AnimAttack(on);
			case 37:
				return $author$project$Message$DebugLeft(on);
			case 38:
				return $author$project$Message$DebugUp(on);
			case 39:
				return $author$project$Message$DebugRight(on);
			case 40:
				return $author$project$Message$DebugDown(on);
			case 13:
				return $author$project$Message$ExitDebugMode;
			default:
				return $author$project$Message$Noop;
		}
	});
var $author$project$Main$keyRage = F2(
	function (on, keycode) {
		switch (keycode) {
			case 68:
				return A2($author$project$Message$AnimWalk, $author$project$Message$Left, on);
			case 65:
				return A2($author$project$Message$AnimWalk, $author$project$Message$Right, on);
			case 32:
				return $author$project$Message$AnimCharge(on);
			case 66:
				return $author$project$Message$AnimCharge(on);
			case 74:
				return $author$project$Message$AnimAttack(on);
			default:
				return $author$project$Message$Noop;
		}
	});
var $elm$browser$Browser$AnimationManager$Delta = function (a) {
	return {$: 'Delta', a: a};
};
var $elm$browser$Browser$AnimationManager$State = F3(
	function (subs, request, oldTime) {
		return {oldTime: oldTime, request: request, subs: subs};
	});
var $elm$browser$Browser$AnimationManager$init = $elm$core$Task$succeed(
	A3($elm$browser$Browser$AnimationManager$State, _List_Nil, $elm$core$Maybe$Nothing, 0));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$browser$Browser$AnimationManager$now = _Browser_now(_Utils_Tuple0);
var $elm$browser$Browser$AnimationManager$rAF = _Browser_rAF(_Utils_Tuple0);
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$browser$Browser$AnimationManager$onEffects = F3(
	function (router, subs, _v0) {
		var request = _v0.request;
		var oldTime = _v0.oldTime;
		var _v1 = _Utils_Tuple2(request, subs);
		if (_v1.a.$ === 'Nothing') {
			if (!_v1.b.b) {
				var _v2 = _v1.a;
				return $elm$browser$Browser$AnimationManager$init;
			} else {
				var _v4 = _v1.a;
				return A2(
					$elm$core$Task$andThen,
					function (pid) {
						return A2(
							$elm$core$Task$andThen,
							function (time) {
								return $elm$core$Task$succeed(
									A3(
										$elm$browser$Browser$AnimationManager$State,
										subs,
										$elm$core$Maybe$Just(pid),
										time));
							},
							$elm$browser$Browser$AnimationManager$now);
					},
					$elm$core$Process$spawn(
						A2(
							$elm$core$Task$andThen,
							$elm$core$Platform$sendToSelf(router),
							$elm$browser$Browser$AnimationManager$rAF)));
			}
		} else {
			if (!_v1.b.b) {
				var pid = _v1.a.a;
				return A2(
					$elm$core$Task$andThen,
					function (_v3) {
						return $elm$browser$Browser$AnimationManager$init;
					},
					$elm$core$Process$kill(pid));
			} else {
				return $elm$core$Task$succeed(
					A3($elm$browser$Browser$AnimationManager$State, subs, request, oldTime));
			}
		}
	});
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$browser$Browser$AnimationManager$onSelfMsg = F3(
	function (router, newTime, _v0) {
		var subs = _v0.subs;
		var oldTime = _v0.oldTime;
		var send = function (sub) {
			if (sub.$ === 'Time') {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(
						$elm$time$Time$millisToPosix(newTime)));
			} else {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(newTime - oldTime));
			}
		};
		return A2(
			$elm$core$Task$andThen,
			function (pid) {
				return A2(
					$elm$core$Task$andThen,
					function (_v1) {
						return $elm$core$Task$succeed(
							A3(
								$elm$browser$Browser$AnimationManager$State,
								subs,
								$elm$core$Maybe$Just(pid),
								newTime));
					},
					$elm$core$Task$sequence(
						A2($elm$core$List$map, send, subs)));
			},
			$elm$core$Process$spawn(
				A2(
					$elm$core$Task$andThen,
					$elm$core$Platform$sendToSelf(router),
					$elm$browser$Browser$AnimationManager$rAF)));
	});
var $elm$browser$Browser$AnimationManager$Time = function (a) {
	return {$: 'Time', a: a};
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$browser$Browser$AnimationManager$subMap = F2(
	function (func, sub) {
		if (sub.$ === 'Time') {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Time(
				A2($elm$core$Basics$composeL, func, tagger));
		} else {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Delta(
				A2($elm$core$Basics$composeL, func, tagger));
		}
	});
_Platform_effectManagers['Browser.AnimationManager'] = _Platform_createManager($elm$browser$Browser$AnimationManager$init, $elm$browser$Browser$AnimationManager$onEffects, $elm$browser$Browser$AnimationManager$onSelfMsg, 0, $elm$browser$Browser$AnimationManager$subMap);
var $elm$browser$Browser$AnimationManager$subscription = _Platform_leaf('Browser.AnimationManager');
var $elm$browser$Browser$AnimationManager$onAnimationFrameDelta = function (tagger) {
	return $elm$browser$Browser$AnimationManager$subscription(
		$elm$browser$Browser$AnimationManager$Delta(tagger));
};
var $elm$browser$Browser$Events$onAnimationFrameDelta = $elm$browser$Browser$AnimationManager$onAnimationFrameDelta;
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onKeyDown = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keydown');
var $elm$browser$Browser$Events$onKeyUp = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keyup');
var $elm$browser$Browser$Events$Window = {$: 'Window'};
var $elm$browser$Browser$Events$onResize = function (func) {
	return A3(
		$elm$browser$Browser$Events$on,
		$elm$browser$Browser$Events$Window,
		'resize',
		A2(
			$elm$json$Json$Decode$field,
			'target',
			A3(
				$elm$json$Json$Decode$map2,
				func,
				A2($elm$json$Json$Decode$field, 'innerWidth', $elm$json$Json$Decode$int),
				A2($elm$json$Json$Decode$field, 'innerHeight', $elm$json$Json$Decode$int))));
};
var $author$project$Main$subscriptions = function (model) {
	return _Utils_eq(model.player.mood, $author$project$Model$Rage) ? $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$elm$browser$Browser$Events$onAnimationFrameDelta($author$project$Message$Tick),
				$elm$browser$Browser$Events$onKeyUp(
				A2(
					$elm$json$Json$Decode$map,
					$author$project$Main$keyRage(false),
					$elm$html$Html$Events$keyCode)),
				$elm$browser$Browser$Events$onKeyDown(
				A2(
					$elm$json$Json$Decode$map,
					$author$project$Main$keyRage(true),
					$elm$html$Html$Events$keyCode)),
				$elm$browser$Browser$Events$onResize($author$project$Message$Resize)
			])) : $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$elm$browser$Browser$Events$onAnimationFrameDelta($author$project$Message$Tick),
				$elm$browser$Browser$Events$onKeyUp(
				A2(
					$elm$json$Json$Decode$map,
					$author$project$Main$keyNormal(false),
					$elm$html$Html$Events$keyCode)),
				$elm$browser$Browser$Events$onKeyDown(
				A2(
					$elm$json$Json$Decode$map,
					$author$project$Main$keyNormal(true),
					$elm$html$Html$Events$keyCode)),
				$elm$browser$Browser$Events$onResize($author$project$Message$Resize)
			]));
};
var $author$project$Model$Charge = {$: 'Charge'};
var $author$project$Model$Jump = {$: 'Jump'};
var $author$project$Model$DebugMode = {$: 'DebugMode'};
var $author$project$Model$Two = {$: 'Two'};
var $author$project$Model$Attack = {$: 'Attack'};
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
var $author$project$AnimState$attack = function (player) {
	return (_Utils_eq(player.anim, $author$project$Model$Stand) || _Utils_eq(player.anim, $author$project$Model$Walk)) ? _Utils_update(
		player,
		{
			anim: $author$project$Model$Attack,
			frame: 0,
			speed: A2($author$project$Model$Vector, 0, 0)
		}) : player;
};
var $author$project$Animate$attackRange = function (character) {
	var pos = character.pos;
	var dx = character.speed.x * 2000;
	return _Utils_update(
		pos,
		{x1: pos.x1 + dx, x2: pos.x2 + dx});
};
var $elm$core$Basics$ge = _Utils_ge;
var $author$project$Collision$projectionOverlap = F4(
	function (min, max, pos1, pos2) {
		return (_Utils_cmp(
			min(pos1),
			max(pos2)) < 0) && (_Utils_cmp(
			max(pos1),
			min(pos2)) > 0);
	});
var $author$project$AnimState$stand = function (player) {
	return _Utils_update(
		player,
		{
			anim: $author$project$Model$Stand,
			frame: 0,
			jumpdir: $author$project$Message$Up,
			speed: A2($author$project$Model$Vector, 0, 0)
		});
};
var $author$project$Animate$attackPlayer = F2(
	function (player, character) {
		var attackPos = $author$project$Animate$attackRange(character);
		return (_Utils_eq(character.anim, $author$project$Model$Attack) && (character.frame >= 30)) ? $author$project$AnimState$stand(character) : (A2(
			$elm$core$List$any,
			function (pos) {
				return A4(
					$author$project$Collision$projectionOverlap,
					function ($) {
						return $.x1;
					},
					function ($) {
						return $.x2;
					},
					attackPos,
					pos) && A4(
					$author$project$Collision$projectionOverlap,
					function ($) {
						return $.y1;
					},
					function ($) {
						return $.y2;
					},
					attackPos,
					pos);
			},
			player.collisionPos) ? ((_Utils_eq(character.anim, $author$project$Model$Stand) && (character.frame < 200)) ? character : $author$project$AnimState$attack(character)) : character);
	});
var $author$project$Model$Attacked = {$: 'Attacked'};
var $author$project$AnimState$attacked = F2(
	function (speed, player) {
		return _Utils_update(
			player,
			{anim: $author$project$Model$Attacked, frame: 0, speed: speed});
	});
var $author$project$Animate$loseBlood = F2(
	function (damage, player) {
		var hp = player.hp - damage;
		return _Utils_update(
			player,
			{hp: hp});
	});
var $author$project$Animate$attackedByCharacter = F2(
	function (character, player) {
		var attackPos = $author$project$Animate$attackRange(character);
		return _Utils_eq(character.anim, $author$project$Model$Attack) ? (_Utils_eq(character.direction, $author$project$Message$Left) ? A2(
			$author$project$Animate$loseBlood,
			0.08,
			A2(
				$author$project$AnimState$attacked,
				A2($author$project$Model$Vector, -0.2, 0),
				player)) : A2(
			$author$project$Animate$loseBlood,
			0.08,
			A2(
				$author$project$AnimState$attacked,
				A2($author$project$Model$Vector, 0.2, 0),
				player))) : player;
	});
var $author$project$Animate$attackedByCharacters = F2(
	function (characters, player) {
		return A3($elm$core$List$foldl, $author$project$Animate$attackedByCharacter, player, characters);
	});
var $author$project$Animate$playerAttackRange = function (player) {
	var pos = player.pos;
	var dx = _Utils_eq(player.direction, $author$project$Message$Left) ? (-80) : 80;
	return _Utils_update(
		pos,
		{x1: pos.x1 + dx, x2: pos.x2 + dx});
};
var $author$project$Animate$attackedByPlayer = F2(
	function (player, character) {
		var attackPos = $author$project$Animate$playerAttackRange(player);
		return _Utils_eq(player.anim, $author$project$Model$Attack) && A2(
			$elm$core$List$any,
			function (pos) {
				return A4(
					$author$project$Collision$projectionOverlap,
					function ($) {
						return $.x1;
					},
					function ($) {
						return $.x2;
					},
					attackPos,
					pos) && A4(
					$author$project$Collision$projectionOverlap,
					function ($) {
						return $.y1;
					},
					function ($) {
						return $.y2;
					},
					attackPos,
					pos);
			},
			character.collisionPos);
	});
var $author$project$Model$Grovel = {$: 'Grovel'};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $author$project$Collision$downImpact = F4(
	function (speed, time, posList, playerPos) {
		return !$elm$core$List$isEmpty(
			A2(
				$elm$core$List$filter,
				function (pos) {
					return (_Utils_cmp(playerPos.y2 - (speed.y * time), pos.y1) < 1) && (_Utils_cmp(playerPos.y2, pos.y1) > -1);
				},
				A2(
					$elm$core$List$filter,
					A3(
						$author$project$Collision$projectionOverlap,
						function ($) {
							return $.x1;
						},
						function ($) {
							return $.x2;
						},
						playerPos),
					posList)));
	});
var $author$project$AnimState$grovel = function (player) {
	return _Utils_update(
		player,
		{
			anim: $author$project$Model$Grovel,
			frame: 0,
			speed: A2($author$project$Model$Vector, 0, 0)
		});
};
var $elm$core$Basics$pow = _Basics_pow;
var $author$project$AnimState$jump = function (player) {
	var time = (player.chargetime > 2000) ? 2 : (player.chargetime / 1000);
	var _v0 = player.jumpdir;
	switch (_v0.$) {
		case 'L':
			return _Utils_update(
				player,
				{
					anim: $author$project$Model$Jump,
					frame: 0,
					speed: A2(
						$author$project$Model$Vector,
						-0.5,
						((0.25 * A2($elm$core$Basics$pow, time, 3)) - (0.75 * A2($elm$core$Basics$pow, time, 2))) - 0.25)
				});
		case 'R':
			return _Utils_update(
				player,
				{
					anim: $author$project$Model$Jump,
					frame: 0,
					speed: A2(
						$author$project$Model$Vector,
						0.5,
						((0.25 * A2($elm$core$Basics$pow, time, 3)) - (0.75 * A2($elm$core$Basics$pow, time, 2))) - 0.25)
				});
		default:
			return _Utils_update(
				player,
				{
					anim: $author$project$Model$Jump,
					frame: 0,
					speed: A2(
						$author$project$Model$Vector,
						0,
						((0.25 * A2($elm$core$Basics$pow, time, 3)) - (0.75 * A2($elm$core$Basics$pow, time, 2))) - 0.25)
				});
	}
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$Animate$nextPos = F3(
	function (speed, time, pos) {
		var dy = speed.y * time;
		var dx = speed.x * time;
		return A4($author$project$MapSetting$Pos, pos.x1 + dx, pos.x2 + dx, pos.y1 + dy, pos.y2 + dy);
	});
var $author$project$Animate$changeAnim = F3(
	function (bricks, time, player) {
		var posList = A2(
			$elm$core$List$map,
			function ($) {
				return $.pos;
			},
			bricks);
		var playerPos = A2(
			$elm$core$List$map,
			A2($author$project$Animate$nextPos, player.speed, time),
			player.collisionPos);
		var newplayer = _Utils_update(
			player,
			{chargetime: 0});
		return _Utils_eq(player.anim, $author$project$Model$Charge) ? player : (_Utils_eq(player.anim, $author$project$Model$Grovel) ? $author$project$AnimState$grovel(player) : ((_Utils_eq(player.anim, $author$project$Model$Jump) && (player.chargetime > 0)) ? $author$project$AnimState$jump(newplayer) : (((_Utils_eq(player.anim, $author$project$Model$Attack) && (player.frame >= 30)) || ((_Utils_eq(player.anim, $author$project$Model$Crouch) && (player.frame >= 60)) || (_Utils_eq(player.anim, $author$project$Model$Attacked) && (player.frame >= 60)))) ? $author$project$AnimState$stand(player) : ((_Utils_eq(player.anim, $author$project$Model$Walk) && ((!(!player.speed.y)) && (!A2(
			$elm$core$List$any,
			A3($author$project$Collision$downImpact, player.speed, time, posList),
			player.collisionPos)))) ? _Utils_update(
			newplayer,
			{anim: $author$project$Model$Jump}) : newplayer))));
	});
var $author$project$Model$CG1_1 = {$: 'CG1_1'};
var $author$project$Model$CG1_2 = {$: 'CG1_2'};
var $author$project$Model$CG1_3 = {$: 'CG1_3'};
var $author$project$Model$CG1_4 = {$: 'CG1_4'};
var $author$project$Model$CG2_1 = {$: 'CG2_1'};
var $author$project$Model$CG2_2 = {$: 'CG2_2'};
var $author$project$Model$CG3_1 = {$: 'CG3_1'};
var $author$project$Model$CG5_1 = {$: 'CG5_1'};
var $author$project$Model$CG5_2 = {$: 'CG5_2'};
var $author$project$Model$CG6_1 = {$: 'CG6_1'};
var $author$project$Model$CG6_2 = {$: 'CG6_2'};
var $author$project$Model$DiscoverI = {$: 'DiscoverI'};
var $author$project$Model$DiscoverII = {$: 'DiscoverII'};
var $author$project$Model$End = {$: 'End'};
var $author$project$Model$One = {$: 'One'};
var $author$project$Model$Story1_1 = {$: 'Story1_1'};
var $author$project$Model$Story1_2 = {$: 'Story1_2'};
var $author$project$Model$Story1_3 = {$: 'Story1_3'};
var $author$project$Model$Story1_4 = {$: 'Story1_4'};
var $author$project$Model$Story2_2 = {$: 'Story2_2'};
var $author$project$Model$Story5_2 = {$: 'Story5_2'};
var $author$project$Model$Three = {$: 'Three'};
var $author$project$ChangeState$changeCGandStory = F2(
	function (time, model) {
		var cgtime = model.cgtime - time;
		if (model.cgtime >= 0) {
			return _Utils_update(
				model,
				{cgtime: cgtime});
		} else {
			var _v0 = model.state;
			switch (_v0.$) {
				case 'LOGO':
					return (!model.player.teachtextstate) ? _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$Story1_1}) : _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$End});
				case 'Story1_1':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$CG1_1});
				case 'CG1_1':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$Story1_2});
				case 'Story1_2':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$CG1_2});
				case 'CG1_2':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$Story1_3});
				case 'Story1_3':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$CG1_3});
				case 'CG1_3':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$Story1_4});
				case 'Story1_4':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$CG1_4});
				case 'CG1_4':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$One});
				case 'Story2_1':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$CG2_1});
				case 'CG2_1':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$Story2_2});
				case 'Story2_2':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$CG2_2});
				case 'CG2_2':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$DiscoverI});
				case 'Story3_1':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$CG3_1});
				case 'CG3_1':
					return _Utils_update(
						model,
						{cgtime: 500, speedAI: $author$project$Model$initSpeedAI, state: $author$project$Model$Two, time: 0});
				case 'Story4_1':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$DiscoverII});
				case 'Story5_1':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$CG5_1});
				case 'CG5_1':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$Story5_2});
				case 'Story5_2':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$CG5_2});
				case 'CG5_2':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$Three});
				case 'Story6_1':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$CG6_1});
				case 'CG6_1':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$CG6_2});
				case 'CG6_2':
					return _Utils_update(
						model,
						{cgtime: 500, state: $author$project$Model$LOGO});
				default:
					return model;
			}
		}
	});
var $author$project$Animate$changeCharactersAndNpcs = F3(
	function (characters, npcs, map) {
		return _Utils_update(
			map,
			{characters: characters, npcs: npcs});
	});
var $author$project$Animate$changeChargeTime = F2(
	function (time, player) {
		var newchargetime = player.chargetime + time;
		return _Utils_eq(player.anim, $author$project$Model$Charge) ? _Utils_update(
			player,
			{chargetime: newchargetime}) : _Utils_update(
			player,
			{chargetime: 0});
	});
var $author$project$Animate$changeFrame = F2(
	function (time, player) {
		return _Utils_update(
			player,
			{frame: player.frame + 1});
	});
var $author$project$Text$changeNPCText = F2(
	function (model, npc) {
		var _v0 = model.state;
		switch (_v0.$) {
			case 'One':
				var _v1 = npc.count;
				switch (_v1) {
					case 1:
						return _Utils_update(
							npc,
							{text: 'Nice to see you... Again?', textframe: -300});
					case 2:
						return _Utils_update(
							npc,
							{text: 'Nice to see you again... Again...', textframe: -300});
					case 3:
						return _Utils_update(
							npc,
							{text: 'Hey, do you really want to escape??', textframe: -300});
					case 4:
						return _Utils_update(
							npc,
							{text: 'Why don\'t you just stay here?', textframe: -300});
					default:
						return _Utils_update(
							npc,
							{text: npc.text});
				}
			case 'DiscoverI':
				var _v2 = npc.count;
				switch (_v2) {
					case 1:
						return _Utils_update(
							npc,
							{text: 'Hey!!! Control your power carefully!!!', textframe: -300});
					case 2:
						return _Utils_update(
							npc,
							{text: 'Fine, if you don\'t be careful... Hahaha....', textframe: -300});
					case 3:
						return _Utils_update(
							npc,
							{text: 'OMG, is that XUAN????????', textframe: -300});
					default:
						return _Utils_update(
							npc,
							{text: npc.text});
				}
			case 'Two':
				var _v3 = npc.count;
				if (_v3 === 1) {
					return _Utils_update(
						npc,
						{text: 'You have no chance to win XUAN anymore....', textframe: -300});
				} else {
					return _Utils_update(
						npc,
						{text: npc.text});
				}
			case 'DiscoverII':
				var _v4 = npc.count;
				switch (_v4) {
					case 1:
						return _Utils_update(
							npc,
							{text: 'I\'m glad to see you!', textframe: -300});
					case 2:
						return _Utils_update(
							npc,
							{text: 'I\'m glad to see you, AGAIN!', textframe: -300});
					case 3:
						return _Utils_update(
							npc,
							{text: 'Come on, again? AGAIN!??', textframe: -300});
					default:
						return _Utils_update(
							npc,
							{text: npc.text});
				}
			case 'Three':
				var _v5 = npc.count;
				switch (_v5) {
					case 1:
						return _Utils_update(
							npc,
							{text: 'Don\'t you want to revenge?', textframe: -300});
					case 2:
						return _Utils_update(
							npc,
							{text: 'I can\'t understand you, why you even being here?', textframe: -300});
					default:
						return _Utils_update(
							npc,
							{text: npc.text});
				}
			default:
				return _Utils_update(
					npc,
					{text: npc.text});
		}
	});
var $author$project$Animate$changePos = F2(
	function (time, player) {
		var pos = A3($author$project$Animate$nextPos, player.speed, time, player.pos);
		var collisionPos = A2(
			$elm$core$List$map,
			A2($author$project$Animate$nextPos, player.speed, time),
			player.collisionPos);
		return _Utils_update(
			player,
			{collisionPos: collisionPos, pos: pos});
	});
var $author$project$AnimState$normal = function (player) {
	return _Utils_update(
		player,
		{
			frame: 0,
			mood: $author$project$Model$Normal,
			speed: A2($author$project$Model$Vector, 0, 0)
		});
};
var $author$project$Animate$changeRageTime = F2(
	function (time, player) {
		var punishtime = player.inrage ? (7500 * player.ragecount) : (5000 * player.ragecount);
		var newragetime = player.ragetime + time;
		var newplayer = $author$project$AnimState$normal(player);
		return ((_Utils_cmp(player.ragetime, punishtime) < 0) && _Utils_eq(player.mood, $author$project$Model$Rage)) ? _Utils_update(
			player,
			{ragetime: newragetime}) : ((_Utils_cmp(player.ragetime, punishtime) > 0) ? _Utils_update(
			newplayer,
			{hp: 10 - (1 * player.ragecount), ragetime: 0}) : player);
	});
var $author$project$Collision$leftImpact = F4(
	function (speed, time, posList, playerPos) {
		return !$elm$core$List$isEmpty(
			A2(
				$elm$core$List$filter,
				function (pos) {
					return (_Utils_cmp(playerPos.x1 - (speed.x * time), pos.x2) > 0) && (_Utils_cmp(playerPos.x1, pos.x2) < 0);
				},
				A2(
					$elm$core$List$filter,
					A3(
						$author$project$Collision$projectionOverlap,
						function ($) {
							return $.y1;
						},
						function ($) {
							return $.y2;
						},
						playerPos),
					posList)));
	});
var $author$project$Collision$rightImpact = F4(
	function (speed, time, posList, playerPos) {
		return !$elm$core$List$isEmpty(
			A2(
				$elm$core$List$filter,
				function (pos) {
					return (_Utils_cmp(playerPos.x2 - (speed.x * time), pos.x1) < 0) && (_Utils_cmp(playerPos.x2, pos.x1) > 0);
				},
				A2(
					$elm$core$List$filter,
					A3(
						$author$project$Collision$projectionOverlap,
						function ($) {
							return $.y1;
						},
						function ($) {
							return $.y2;
						},
						playerPos),
					posList)));
	});
var $author$project$Collision$upImpact = F4(
	function (speed, time, posList, playerPos) {
		return !$elm$core$List$isEmpty(
			A2(
				$elm$core$List$filter,
				function (pos) {
					return (_Utils_cmp(playerPos.y1 - (speed.y * time), pos.y2) > -1) && (_Utils_cmp(playerPos.y1, pos.y2) < 1);
				},
				A2(
					$elm$core$List$filter,
					A3(
						$author$project$Collision$projectionOverlap,
						function ($) {
							return $.x1;
						},
						function ($) {
							return $.x2;
						},
						playerPos),
					posList)));
	});
var $author$project$Animate$changeSpeed = F3(
	function (time, bricks, player) {
		var posList = A2(
			$elm$core$List$map,
			function ($) {
				return $.pos;
			},
			bricks);
		var playerPos = A2(
			$elm$core$List$map,
			A2($author$project$Animate$nextPos, player.speed, time),
			player.collisionPos);
		var dy = A2(
			$elm$core$List$any,
			A3($author$project$Collision$upImpact, player.speed, time, posList),
			playerPos) ? ((-1.8) * player.speed.y) : 0.03;
		var dx = (A2(
			$elm$core$List$any,
			A3($author$project$Collision$rightImpact, player.speed, time, posList),
			playerPos) || A2(
			$elm$core$List$any,
			A3($author$project$Collision$leftImpact, player.speed, time, posList),
			playerPos)) ? ((_Utils_eq(player.anim, $author$project$Model$Walk) || _Utils_eq(player.anim, $author$project$Model$Attacked)) ? (-player.speed.x) : ((-1.8) * player.speed.x)) : 0;
		var speed = A2($author$project$Model$Vector, player.speed.x + dx, player.speed.y + dy);
		return _Utils_update(
			player,
			{speed: speed});
	});
var $author$project$Model$Story2_1 = {$: 'Story2_1'};
var $author$project$Model$Story3_1 = {$: 'Story3_1'};
var $author$project$Model$Story4_1 = {$: 'Story4_1'};
var $author$project$Model$Story5_1 = {$: 'Story5_1'};
var $author$project$Model$Story6_1 = {$: 'Story6_1'};
var $author$project$ChangeState$arriveExit = F2(
	function (model, player) {
		return (_Utils_cmp(player.pos.x2, model.map.exit.x1) > -1) && ((_Utils_cmp(player.pos.x1, model.map.exit.x2) < 1) && ((_Utils_cmp(player.pos.y1, model.map.exit.y2) < 1) && (_Utils_cmp(player.pos.y2, model.map.exit.y1) > -1)));
	});
var $author$project$MapSetting$birdPosList2 = _List_Nil;
var $author$project$MapSetting$brickPosList2 = _List_fromArray(
	[
		A4($author$project$MapSetting$Pos, 550, 700, 4585, 4735),
		A4($author$project$MapSetting$Pos, 900, 1050, 4585, 4735),
		A4($author$project$MapSetting$Pos, 550, 700, 4200, 4450),
		A4($author$project$MapSetting$Pos, 900, 1050, 4200, 4450),
		A4($author$project$MapSetting$Pos, 100, 200, 3900, 4100),
		A4($author$project$MapSetting$Pos, 1400, 1500, 3900, 4100),
		A4($author$project$MapSetting$Pos, 550, 750, 3700, 3800),
		A4($author$project$MapSetting$Pos, 850, 1050, 3700, 3800),
		A4($author$project$MapSetting$Pos, 725, 875, 3450, 3515),
		A4($author$project$MapSetting$Pos, 250, 600, 3150, 3250),
		A4($author$project$MapSetting$Pos, 1000, 1350, 3150, 3250),
		A4($author$project$MapSetting$Pos, 550, 1050, 2900, 3000),
		A4($author$project$MapSetting$Pos, 100, 300, 2700, 2765),
		A4($author$project$MapSetting$Pos, 750, 1200, 2350, 2450),
		A4($author$project$MapSetting$Pos, 1300, 1500, 2100, 2200),
		A4($author$project$MapSetting$Pos, 1300, 1500, 1750, 1850),
		A4($author$project$MapSetting$Pos, 500, 750, 2000, 2100),
		A4($author$project$MapSetting$Pos, 500, 750, 1650, 1750),
		A4($author$project$MapSetting$Pos, 1000, 1050, 850, 2200),
		A4($author$project$MapSetting$Pos, 1300, 1500, 1400, 1500),
		A4($author$project$MapSetting$Pos, 1300, 1500, 1000, 1100),
		A4($author$project$MapSetting$Pos, 500, 750, 1300, 1400),
		A4($author$project$MapSetting$Pos, 100, 300, 1100, 1250),
		A4($author$project$MapSetting$Pos, 575, 1200, 785, 850),
		A4($author$project$MapSetting$Pos, 875, 1500, 500, 600),
		A4($author$project$MapSetting$Pos, 700, 900, 250, 325)
	]);
var $author$project$MapSetting$brickWallList2 = _List_fromArray(
	[
		A4($author$project$MapSetting$Pos, 0, 100, 0, 4800),
		A4($author$project$MapSetting$Pos, 100, 1500, 4735, 4800),
		A4($author$project$MapSetting$Pos, 1500, 1600, 0, 4800)
	]);
var $author$project$MapSetting$characterPosList2 = _List_fromArray(
	[
		A4($author$project$MapSetting$Pos, 750, 850, 150, 250)
	]);
var $author$project$MapSetting$exitPos2 = A4($author$project$MapSetting$Pos, 750, 850, 150, 250);
var $author$project$Model$initBirds2 = function (posList) {
	return A2(
		$elm$core$List$map,
		function (pos) {
			return {
				anim: $author$project$Model$Stand,
				direction: $author$project$Message$Right,
				frame: 0,
				pos: pos,
				speed: A2($author$project$Model$Vector, 0, 0)
			};
		},
		posList);
};
var $author$project$Model$initNpcs2 = function (posList) {
	return A2(
		$elm$core$List$map,
		function (pos) {
			return {
				anim: $author$project$Model$Stand,
				count: 0,
				direction: $author$project$Message$Left,
				frame: 0,
				pos: pos,
				speed: A2($author$project$Model$Vector, 0, 0),
				text: 'Go HIGH, Go FAST! Don\'t let XUAN takeover your body!!!',
				textframe: 0
			};
		},
		posList);
};
var $author$project$MapSetting$npcPosList2 = _List_Nil;
var $author$project$Model$initMap2 = {
	birds: $author$project$Model$initBirds2($author$project$MapSetting$birdPosList2),
	bricks: $author$project$Model$initBricks($author$project$MapSetting$brickPosList2),
	characters: $author$project$Model$initCharacters($author$project$MapSetting$characterPosList2),
	exit: $author$project$MapSetting$exitPos2,
	npcs: $author$project$Model$initNpcs2($author$project$MapSetting$npcPosList2),
	wallbricks: $author$project$Model$initBricks($author$project$MapSetting$brickWallList2)
};
var $author$project$MapSetting$birdPosList3 = _List_Nil;
var $author$project$MapSetting$brickPosList3 = _List_fromArray(
	[
		A4($author$project$MapSetting$Pos, 500, 800, 4600, 4735),
		A4($author$project$MapSetting$Pos, 100, 350, 4250, 4350),
		A4($author$project$MapSetting$Pos, 850, 1050, 4200, 4300),
		A4($author$project$MapSetting$Pos, 1300, 1500, 3975, 4075),
		A4($author$project$MapSetting$Pos, 250, 500, 3835, 3900),
		A4($author$project$MapSetting$Pos, 1000, 1200, 3650, 3735),
		A4($author$project$MapSetting$Pos, 650, 850, 3535, 3600),
		A4($author$project$MapSetting$Pos, 250, 800, 3175, 3275),
		A4($author$project$MapSetting$Pos, 1100, 1400, 3315, 3415),
		A4($author$project$MapSetting$Pos, 100, 350, 2900, 3000),
		A4($author$project$MapSetting$Pos, 1200, 1500, 2935, 3000),
		A4($author$project$MapSetting$Pos, 900, 1000, 2965, 3000),
		A4($author$project$MapSetting$Pos, 1350, 1500, 2700, 2765),
		A4($author$project$MapSetting$Pos, 900, 1200, 2700, 2765),
		A4($author$project$MapSetting$Pos, 550, 775, 2600, 2670),
		A4($author$project$MapSetting$Pos, 500, 550, 2575, 2670),
		A4($author$project$MapSetting$Pos, 250, 550, 2515, 2575),
		A4($author$project$MapSetting$Pos, 950, 1300, 2200, 2225),
		A4($author$project$MapSetting$Pos, 950, 1150, 2225, 2425),
		A4($author$project$MapSetting$Pos, 950, 1300, 2425, 2450),
		A4($author$project$MapSetting$Pos, 1250, 1300, 2100, 2200),
		A4($author$project$MapSetting$Pos, 1350, 1500, 1800, 1865),
		A4($author$project$MapSetting$Pos, 600, 950, 1650, 2300),
		A4($author$project$MapSetting$Pos, 100, 600, 2200, 2300),
		A4($author$project$MapSetting$Pos, 350, 600, 1900, 1965),
		A4($author$project$MapSetting$Pos, 100, 250, 1705, 1770),
		A4($author$project$MapSetting$Pos, 600, 1250, 1550, 1650),
		A4($author$project$MapSetting$Pos, 250, 750, 1200, 1300),
		A4($author$project$MapSetting$Pos, 1250, 1500, 1250, 1350),
		A4($author$project$MapSetting$Pos, 900, 1250, 950, 1050),
		A4($author$project$MapSetting$Pos, 100, 600, 700, 800),
		A4($author$project$MapSetting$Pos, 400, 1200, 300, 400),
		A4($author$project$MapSetting$Pos, -1600, 0, 1900, 2000),
		A4($author$project$MapSetting$Pos, -1600, -1500, 2000, 4000),
		A4($author$project$MapSetting$Pos, -1500, -100, 3900, 4000),
		A4($author$project$MapSetting$Pos, -100, 0, 2200, 4000),
		A4($author$project$MapSetting$Pos, -250, -100, 2530, 3200),
		A4($author$project$MapSetting$Pos, -400, -250, 2610, 3200),
		A4($author$project$MapSetting$Pos, -550, -400, 2690, 3200),
		A4($author$project$MapSetting$Pos, -700, -550, 2770, 3200),
		A4($author$project$MapSetting$Pos, -850, -700, 2850, 3200),
		A4($author$project$MapSetting$Pos, -1000, -850, 2930, 3200),
		A4($author$project$MapSetting$Pos, -1150, -1000, 3010, 3200),
		A4($author$project$MapSetting$Pos, -1300, -1150, 3090, 3200),
		A4($author$project$MapSetting$Pos, -1500, -1200, 3500, 3900),
		A4($author$project$MapSetting$Pos, -1200, -900, 3700, 3900),
		A4($author$project$MapSetting$Pos, -600, -100, 3800, 3900)
	]);
var $author$project$MapSetting$brickWallList3 = _List_fromArray(
	[
		A4($author$project$MapSetting$Pos, 0, 100, 0, 2000),
		A4($author$project$MapSetting$Pos, 0, 100, 2200, 4800),
		A4($author$project$MapSetting$Pos, 100, 1500, 4735, 4800),
		A4($author$project$MapSetting$Pos, 1500, 1600, 0, 4800)
	]);
var $author$project$MapSetting$characterPosList3 = _List_fromArray(
	[
		A4($author$project$MapSetting$Pos, 175, 275, 4150, 4250),
		A4($author$project$MapSetting$Pos, 300, 400, 3735, 3835),
		A4($author$project$MapSetting$Pos, 300, 400, 3075, 3175),
		A4($author$project$MapSetting$Pos, 1200, 1300, 3215, 3315),
		A4($author$project$MapSetting$Pos, 1300, 1400, 2835, 2935),
		A4($author$project$MapSetting$Pos, 1000, 1100, 2600, 2700),
		A4($author$project$MapSetting$Pos, 620, 720, 2500, 2600),
		A4($author$project$MapSetting$Pos, 1050, 1150, 2100, 2200),
		A4($author$project$MapSetting$Pos, 150, 250, 2100, 2200),
		A4($author$project$MapSetting$Pos, 400, 500, 1800, 1900),
		A4($author$project$MapSetting$Pos, 125, 225, 1605, 1705),
		A4($author$project$MapSetting$Pos, 650, 750, 1450, 1550),
		A4($author$project$MapSetting$Pos, 800, 900, 1450, 1550),
		A4($author$project$MapSetting$Pos, 400, 500, 1100, 1200),
		A4($author$project$MapSetting$Pos, 425, 525, 200, 300),
		A4($author$project$MapSetting$Pos, 1075, 1175, 200, 300),
		A4($author$project$MapSetting$Pos, 750, 850, 100, 300),
		A4($author$project$MapSetting$Pos, -500, -400, 3700, 3800),
		A4($author$project$MapSetting$Pos, -300, -200, 3700, 3800)
	]);
var $author$project$MapSetting$exitPos3 = A4($author$project$MapSetting$Pos, 2000, 2100, 0, 100);
var $author$project$Model$initBirds3 = function (posList) {
	return A2(
		$elm$core$List$map,
		function (pos) {
			return {
				anim: $author$project$Model$Stand,
				direction: $author$project$Message$Right,
				frame: 0,
				pos: pos,
				speed: A2($author$project$Model$Vector, 0, 0)
			};
		},
		posList);
};
var $author$project$Model$initNpcs3 = function (posList) {
	return A2(
		$elm$core$List$map,
		function (pos) {
			return {
				anim: $author$project$Model$Stand,
				count: 0,
				direction: $author$project$Message$Left,
				frame: 0,
				pos: pos,
				speed: A2($author$project$Model$Vector, 0, 0),
				text: 'REVENGE?? Interesting...',
				textframe: 0
			};
		},
		posList);
};
var $author$project$MapSetting$npcPosList3 = _List_Nil;
var $author$project$Model$initMap3 = {
	birds: $author$project$Model$initBirds3($author$project$MapSetting$birdPosList3),
	bricks: $author$project$Model$initBricks($author$project$MapSetting$brickPosList3),
	characters: $author$project$Model$initCharacters($author$project$MapSetting$characterPosList3),
	exit: $author$project$MapSetting$exitPos3,
	npcs: $author$project$Model$initNpcs3($author$project$MapSetting$npcPosList3),
	wallbricks: $author$project$Model$initBricks($author$project$MapSetting$brickWallList3)
};
var $author$project$MapSetting$birdPosListDiscoverI = _List_Nil;
var $author$project$MapSetting$brickPosListDiscoverI = _List_fromArray(
	[
		A4($author$project$MapSetting$Pos, 400, 600, 3700, 3765),
		A4($author$project$MapSetting$Pos, 800, 1000, 3475, 3540),
		A4($author$project$MapSetting$Pos, 1200, 1500, 3175, 3275),
		A4($author$project$MapSetting$Pos, 100, 200, 3195, 3395),
		A4($author$project$MapSetting$Pos, 1325, 1375, 2975, 3175),
		A4($author$project$MapSetting$Pos, 650, 950, 2900, 3000),
		A4($author$project$MapSetting$Pos, 775, 825, 2700, 2900),
		A4($author$project$MapSetting$Pos, 100, 400, 2675, 2775),
		A4($author$project$MapSetting$Pos, 225, 275, 2475, 2675),
		A4($author$project$MapSetting$Pos, 350, 650, 2200, 2265),
		A4($author$project$MapSetting$Pos, 850, 1150, 2000, 2065),
		A4($author$project$MapSetting$Pos, 1300, 1500, 1800, 1865),
		A4($author$project$MapSetting$Pos, 300, 1100, 1550, 1650),
		A4($author$project$MapSetting$Pos, 100, 250, 1800, 2200),
		A4($author$project$MapSetting$Pos, 500, 750, 1250, 1400),
		A4($author$project$MapSetting$Pos, 1000, 1250, 950, 1100),
		A4($author$project$MapSetting$Pos, 1400, 1500, 750, 900),
		A4($author$project$MapSetting$Pos, 1150, 1300, 600, 700),
		A4($author$project$MapSetting$Pos, 2500, 2550, 300, 850),
		A4($author$project$MapSetting$Pos, 2650, 2950, 400, 850)
	]);
var $author$project$MapSetting$brickWallListDiscoverI = _List_fromArray(
	[
		A4($author$project$MapSetting$Pos, 0, 100, 0, 4000),
		A4($author$project$MapSetting$Pos, 1500, 1600, 750, 4000),
		A4($author$project$MapSetting$Pos, 1600, 1800, 750, 850),
		A4($author$project$MapSetting$Pos, 100, 3200, 0, 100),
		A4($author$project$MapSetting$Pos, 100, 1500, 3935, 4000)
	]);
var $author$project$MapSetting$characterPosListDiscoverI = _List_Nil;
var $author$project$MapSetting$exitPosDiscoverI = A4($author$project$MapSetting$Pos, 1600, 10000, 2400, 3000);
var $author$project$Model$initBirdsDiscoverI = function (posList) {
	return A2(
		$elm$core$List$map,
		function (pos) {
			return {
				anim: $author$project$Model$Stand,
				direction: $author$project$Message$Right,
				frame: 0,
				pos: pos,
				speed: A2($author$project$Model$Vector, 0, 0)
			};
		},
		posList);
};
var $author$project$Model$initNpcsDiscoverI = function (posList) {
	return A2(
		$elm$core$List$map,
		function (pos) {
			return {
				anim: $author$project$Model$Stand,
				count: 0,
				direction: $author$project$Message$Left,
				frame: -500,
				pos: pos,
				speed: A2($author$project$Model$Vector, 0, 0),
				text: 'Be careful, your power is very strong now...',
				textframe: 0
			};
		},
		posList);
};
var $author$project$MapSetting$npcPosListDiscoverI = _List_Nil;
var $author$project$Model$initMapDiscoverI = {
	birds: $author$project$Model$initBirdsDiscoverI($author$project$MapSetting$birdPosListDiscoverI),
	bricks: $author$project$Model$initBricks($author$project$MapSetting$brickPosListDiscoverI),
	characters: $author$project$Model$initCharacters($author$project$MapSetting$characterPosListDiscoverI),
	exit: $author$project$MapSetting$exitPosDiscoverI,
	npcs: $author$project$Model$initNpcsDiscoverI($author$project$MapSetting$npcPosListDiscoverI),
	wallbricks: $author$project$Model$initBricks($author$project$MapSetting$brickWallListDiscoverI)
};
var $author$project$MapSetting$birdPosListDiscoverII = _List_Nil;
var $author$project$MapSetting$brickPosListDiscoverII = _List_fromArray(
	[
		A4($author$project$MapSetting$Pos, 800, 1300, 3600, 3700),
		A4($author$project$MapSetting$Pos, 800, 950, 3250, 3600),
		A4($author$project$MapSetting$Pos, 800, 1300, 3195, 3295),
		A4($author$project$MapSetting$Pos, 200, 600, 3150, 3250),
		A4($author$project$MapSetting$Pos, 100, 200, 2900, 3000),
		A4($author$project$MapSetting$Pos, 350, 550, 2700, 2950),
		A4($author$project$MapSetting$Pos, 100, 350, 2450, 2550),
		A4($author$project$MapSetting$Pos, 550, 800, 2350, 2950),
		A4($author$project$MapSetting$Pos, 1050, 1500, 2845, 3000),
		A4($author$project$MapSetting$Pos, 1400, 1500, 2525, 2625),
		A4($author$project$MapSetting$Pos, 1250, 1400, 2525, 2575),
		A4($author$project$MapSetting$Pos, 100, 400, 2100, 2200),
		A4($author$project$MapSetting$Pos, 100, 550, 2000, 2100),
		A4($author$project$MapSetting$Pos, 200, 300, 1900, 2000),
		A4($author$project$MapSetting$Pos, 100, 200, 1550, 2000),
		A4($author$project$MapSetting$Pos, 1400, 1500, 1550, 1700),
		A4($author$project$MapSetting$Pos, 400, 500, 1300, 1365),
		A4($author$project$MapSetting$Pos, 100, 200, 1050, 1115),
		A4($author$project$MapSetting$Pos, 500, 600, 785, 1150),
		A4($author$project$MapSetting$Pos, 500, 700, 1150, 1650),
		A4($author$project$MapSetting$Pos, 1100, 1500, 1150, 1550),
		A4($author$project$MapSetting$Pos, 800, 1200, 500, 850)
	]);
var $author$project$MapSetting$brickWallListDiscoverII = _List_fromArray(
	[
		A4($author$project$MapSetting$Pos, 0, 100, 0, 4000),
		A4($author$project$MapSetting$Pos, 1500, 1600, 0, 4000),
		A4($author$project$MapSetting$Pos, 100, 1500, 0, 100),
		A4($author$project$MapSetting$Pos, 100, 1500, 3935, 4000)
	]);
var $author$project$MapSetting$characterPosListDiscoverII = _List_Nil;
var $author$project$MapSetting$exitPosDiscoverII = A4($author$project$MapSetting$Pos, 900, 1100, 400, 500);
var $author$project$Model$initBirdsDiscoverII = function (posList) {
	return A2(
		$elm$core$List$map,
		function (pos) {
			return {
				anim: $author$project$Model$Stand,
				direction: $author$project$Message$Right,
				frame: 0,
				pos: pos,
				speed: A2($author$project$Model$Vector, 0, 0)
			};
		},
		posList);
};
var $author$project$Model$initNpcsDiscoverII = function (posList) {
	return A2(
		$elm$core$List$map,
		function (pos) {
			return {
				anim: $author$project$Model$Stand,
				count: 0,
				direction: $author$project$Message$Left,
				frame: 0,
				pos: pos,
				speed: A2($author$project$Model$Vector, 0, 0),
				text: 'Hey, where are you going?',
				textframe: 0
			};
		},
		posList);
};
var $author$project$MapSetting$npcPosListDiscoverII = _List_Nil;
var $author$project$Model$initMapDiscoverII = {
	birds: $author$project$Model$initBirdsDiscoverII($author$project$MapSetting$birdPosListDiscoverII),
	bricks: $author$project$Model$initBricks($author$project$MapSetting$brickPosListDiscoverII),
	characters: $author$project$Model$initCharacters($author$project$MapSetting$characterPosListDiscoverII),
	exit: $author$project$MapSetting$exitPosDiscoverII,
	npcs: $author$project$Model$initNpcsDiscoverII($author$project$MapSetting$npcPosListDiscoverII),
	wallbricks: $author$project$Model$initBricks($author$project$MapSetting$brickWallListDiscoverII)
};
var $author$project$MapSetting$playerPos2 = A4($author$project$MapSetting$Pos, 300, 400, 4630, 4730);
var $author$project$Model$initPlayer2 = function (player) {
	return _Utils_update(
		player,
		{
			anim: $author$project$Model$Crouch,
			collisionPos: $author$project$Model$standcollisionPos($author$project$MapSetting$playerPos2),
			direction: $author$project$Message$Left,
			fallcount: 0,
			frame: 0,
			jumpdir: $author$project$Message$Up,
			pos: $author$project$MapSetting$playerPos2,
			speed: A2($author$project$Model$Vector, 0, 0),
			text: 'I am Song Yuanhuai.',
			textframe: 0
		});
};
var $author$project$MapSetting$playerPos3 = A4($author$project$MapSetting$Pos, 200, 300, 4630, 4730);
var $author$project$Model$initPlayer3 = function (player) {
	return _Utils_update(
		player,
		{
			anim: $author$project$Model$Crouch,
			collisionPos: $author$project$Model$standcollisionPos($author$project$MapSetting$playerPos3),
			direction: $author$project$Message$Left,
			fallcount: 0,
			frame: 0,
			jumpdir: $author$project$Message$Up,
			pos: $author$project$MapSetting$playerPos3,
			speed: A2($author$project$Model$Vector, 0, 0),
			text: 'I am back... FOR REVENGE!',
			textframe: 0
		});
};
var $author$project$MapSetting$playerPosDiscoverI = A4($author$project$MapSetting$Pos, 200, 300, 3830, 3930);
var $author$project$Model$initPlayerDiscoverI = function (player) {
	return _Utils_update(
		player,
		{
			anim: $author$project$Model$Crouch,
			collisionPos: $author$project$Model$standcollisionPos($author$project$MapSetting$playerPosDiscoverI),
			direction: $author$project$Message$Left,
			fallcount: 0,
			frame: 0,
			jumpdir: $author$project$Message$Up,
			pos: $author$project$MapSetting$playerPosDiscoverI,
			speed: A2($author$project$Model$Vector, 0, 0),
			text: 'What\'s going on?',
			textframe: 0
		});
};
var $author$project$MapSetting$playerPosDiscoverII = A4($author$project$MapSetting$Pos, 200, 300, 3830, 3930);
var $author$project$Model$initPlayerDiscoverII = function (player) {
	return _Utils_update(
		player,
		{
			anim: $author$project$Model$Crouch,
			collisionPos: $author$project$Model$standcollisionPos($author$project$MapSetting$playerPosDiscoverII),
			direction: $author$project$Message$Left,
			fallcount: 0,
			frame: 0,
			jumpdir: $author$project$Message$Up,
			pos: $author$project$MapSetting$playerPosDiscoverII,
			speed: A2($author$project$Model$Vector, 0, 0),
			text: 'Life is a series of choices, and you don\'t know about the consequences.',
			textframe: 0
		});
};
var $author$project$ChangeState$changeState = function (model) {
	var playerDiscoverIIWin = $author$project$Model$initPlayerDiscoverII(model.player);
	var playerDiscoverIILose = _Utils_update(
		playerDiscoverIIWin,
		{inrage: true});
	var playerDiscoverI = $author$project$Model$initPlayerDiscoverI(model.player);
	var player3 = $author$project$Model$initPlayer3(model.player);
	var player2 = $author$project$Model$initPlayer2(model.player);
	if (A2($author$project$ChangeState$arriveExit, model, model.player)) {
		var _v0 = model.state;
		switch (_v0.$) {
			case 'One':
				return _Utils_update(
					model,
					{map: $author$project$Model$initMapDiscoverI, player: playerDiscoverI, state: $author$project$Model$Story2_1, time: 0});
			case 'DiscoverI':
				return _Utils_update(
					model,
					{map: $author$project$Model$initMap2, player: player2, state: $author$project$Model$Story3_1, time: 0});
			case 'Two':
				return A2($author$project$ChangeState$arriveExit, model, model.speedAI) ? _Utils_update(
					model,
					{map: $author$project$Model$initMapDiscoverII, player: playerDiscoverIILose, state: $author$project$Model$Story4_1, time: 0}) : _Utils_update(
					model,
					{map: $author$project$Model$initMapDiscoverII, player: playerDiscoverIIWin, state: $author$project$Model$Story4_1, time: 0});
			case 'DiscoverII':
				return _Utils_update(
					model,
					{map: $author$project$Model$initMap3, player: player3, state: $author$project$Model$Story5_1, time: 0});
			case 'Three':
				return _Utils_update(
					model,
					{map: $author$project$Model$initMap1, player: $author$project$Model$initPlayer1, state: $author$project$Model$Story6_1, time: 0});
			default:
				return model;
		}
	} else {
		return model;
	}
};
var $author$project$Text$changeStory = F2(
	function (state, story) {
		switch (state.$) {
			case 'Story1_1':
				return _Utils_update(
					story,
					{storyframe: 0, text: 'Old man：\"Poor kid, to improve the status of your family, your father and I decide to send you to attend the experiments in our sect. For the rest of your life, you will need to practice all kinds' + ('of internal skills and receive all kinds of stimulations. Don’t feel unfair. This is the brutality of the sects. Without sacrifice, there will be no strong sects.\"' + ('A mysterious voice: \"Bullshit! How can anyone believe this old bastard? Sending your apprentice to be a testimony and feel sorry for him? You are one of a kind!\"' + ('Song：\"Who are you? What’s that voice in my head? What’s going on with me?\"' + 'A mysterious voice:\"You’re wondering who I am? My name is XUAN, and I can set you free.\"')))});
			case 'Story1_2':
				return _Utils_update(
					story,
					{storyframe: 0, text: 'Story1_2'});
			case 'Story1_3':
				return _Utils_update(
					story,
					{storyframe: 0, text: 'Story1_3'});
			case 'Story1_4':
				return _Utils_update(
					story,
					{storyframe: 0, text: 'Story1_4'});
			case 'Story2_1':
				return _Utils_update(
					story,
					{storyframe: 0, text: 'Story1_4'});
			case 'Story2_2':
				return _Utils_update(
					story,
					{storyframe: 0, text: 'Story1_4'});
			case 'Story3_1':
				return _Utils_update(
					story,
					{storyframe: 0, text: 'Story1_4'});
			case 'Story4_1':
				return _Utils_update(
					story,
					{storyframe: 0, text: 'Story1_4'});
			case 'Story5_1':
				return _Utils_update(
					story,
					{storyframe: 0, text: 'Story1_4'});
			case 'Story5_2':
				return _Utils_update(
					story,
					{storyframe: 0, text: 'Story1_4'});
			case 'Story6_1':
				return _Utils_update(
					story,
					{storyframe: 0, text: 'Story1_4'});
			default:
				return _Utils_update(
					story,
					{text: story.text});
		}
	});
var $author$project$Text$changeText = F3(
	function (state, speedAI, player) {
		switch (state.$) {
			case 'One':
				return (!_Utils_eq(player.teachtextstate, -1)) ? (((player.pos.x1 >= 0) && (player.pos.x2 <= 1500)) ? ((_Utils_eq(player.anim, $author$project$Model$Stand) && (!player.teachtextstate)) ? _Utils_update(
					player,
					{text: 'Use A and D to move right and left.', textframe: -500}) : ((_Utils_eq(player.anim, $author$project$Model$Walk) && (!player.teachtextstate)) ? _Utils_update(
					player,
					{teachtextstate: 1}) : ((_Utils_eq(player.anim, $author$project$Model$Stand) && (player.teachtextstate === 1)) ? _Utils_update(
					player,
					{text: 'Press Space or B to Jump.', textframe: -500}) : ((_Utils_eq(player.anim, $author$project$Model$Jump) && (player.teachtextstate === 1)) ? _Utils_update(
					player,
					{teachtextstate: 2}) : ((_Utils_eq(player.anim, $author$project$Model$Stand) && (player.teachtextstate === 2)) ? _Utils_update(
					player,
					{text: 'Try to hold it longer...', textframe: -500}) : (((player.chargetime >= 1500) && (player.teachtextstate === 2)) ? _Utils_update(
					player,
					{teachtextstate: 3}) : ((_Utils_eq(player.anim, $author$project$Model$Stand) && (player.teachtextstate === 3)) ? _Utils_update(
					player,
					{teachtextstate: 4, text: 'Now, hold A or D while jumping.', textframe: -500}) : ((_Utils_eq(player.anim, $author$project$Model$Jump) && ((!player.speed.x) && (player.teachtextstate === 4))) ? _Utils_update(
					player,
					{teachtextstate: 5}) : ((_Utils_eq(player.anim, $author$project$Model$Stand) && (player.teachtextstate === 5)) ? _Utils_update(
					player,
					{text: 'You need to hold it until you release the space key!', textframe: -500}) : ((_Utils_eq(player.anim, $author$project$Model$Jump) && ((!(!player.speed.x)) && ((player.pos.y2 >= 3090) && ((player.speed.y > 0) && ((player.teachtextstate === 5) || (player.teachtextstate === 4)))))) ? _Utils_update(
					player,
					{teachtextstate: 6}) : ((player.teachtextstate === 6) ? _Utils_update(
					player,
					{teachtextstate: 7, text: 'You are ready to run away from here... Go right.', textframe: -500}) : _Utils_update(
					player,
					{text: player.text})))))))))))) : (((player.pos.x1 <= 2650) && ((player.pos.x1 >= 2000) && (player.teachtextstate === 7))) ? _Utils_update(
					player,
					{teachtextstate: 8, text: 'See the soliers? Try to attack them using J.', textframe: -500}) : (((player.pos.x1 >= 1500) && (player.teachtextstate < 7)) ? _Utils_update(
					player,
					{teachtextstate: -1, text: 'You don\'t needs more instructions !?? Fine, good luck.....', textframe: -500}) : ((_Utils_eq(player.anim, $author$project$Model$Attack) && (player.teachtextstate === 8)) ? _Utils_update(
					player,
					{teachtextstate: 9}) : ((player.teachtextstate === 9) ? _Utils_update(
					player,
					{teachtextstate: -1, text: 'You have learned everything you need. Now, just go up, up and up...', textframe: -200}) : _Utils_update(
					player,
					{text: player.text})))))) : (((player.pos.y2 <= 970) && ((player.pos.x2 <= 3200) && _Utils_eq(player.anim, $author$project$Model$Stand))) ? _Utils_update(
					player,
					{text: 'Finally', textframe: 0}) : (((player.pos.y2 <= 970) && ((player.pos.x1 >= 3200) && _Utils_eq(player.anim, $author$project$Model$Stand))) ? _Utils_update(
					player,
					{text: 'Cliff... ', textframe: -500}) : _Utils_update(
					player,
					{text: player.text})));
			case 'DiscoverI':
				return ((player.pos.x1 >= 1600) && (player.pos.y1 >= 900)) ? _Utils_update(
					player,
					{text: 'NO!!!', textframe: 0}) : _Utils_update(
					player,
					{text: player.text});
			case 'Two':
				return (_Utils_cmp(player.pos.y1, speedAI.pos.y1 + 500) > 0) ? _Utils_update(
					player,
					{text: 'I need to catch up!', textframe: 0}) : ((player.pos.y2 <= 300) ? _Utils_update(
					player,
					{text: 'No one can control me!', textframe: 0}) : ((speedAI.pos.y2 <= 300) ? _Utils_update(
					player,
					{text: 'No! Please! I don\'t want to...', textframe: 0}) : _Utils_update(
					player,
					{text: player.text})));
			case 'DiscoverII':
				return (player.pos.y2 <= 500) ? _Utils_update(
					player,
					{text: 'It\'s been 10 years... Now I\'m back.', textframe: 0}) : _Utils_update(
					player,
					{text: player.text});
			case 'Three':
				return ((player.pos.y2 >= 1800) && (player.pos.y2 <= 2900)) ? _Utils_update(
					player,
					{text: 'I’m coming for you, Master Luke.', textframe: 0}) : ((player.pos.y2 <= 350) ? _Utils_update(
					player,
					{text: 'You are doomed…', textframe: 0}) : _Utils_update(
					player,
					{text: player.text}));
			default:
				return _Utils_update(
					player,
					{text: player.text});
		}
	});
var $author$project$Animate$changeTextframe = F2(
	function (time, player) {
		return _Utils_update(
			player,
			{textframe: player.textframe + 1});
	});
var $author$project$Animate$chargeEffectTime = F2(
	function (time, player) {
		var newEffectTimeTwo = (player.effecttimeTwo <= 1000) ? (player.effecttimeTwo + (1.25 * time)) : (-(player.effecttimeTwo + (1.25 * time)));
		var newEffectTimeThree = (player.effecttimeThree <= 1000) ? (player.effecttimeThree + (1.5 * time)) : (-(player.effecttimeThree + (1.5 * time)));
		var newEffectTimeOne = (player.effecttimeOne <= 1000) ? (player.effecttimeOne + time) : (-(player.effecttimeOne + time));
		var newEffectTimeFour = (player.effecttimeFour <= 1000) ? (player.effecttimeFour + (1.75 * time)) : (-(player.effecttimeFour + (1.75 * time)));
		var newEffectTimeFive = (player.effecttimeFive <= 1000) ? (player.effecttimeFive + (2 * time)) : (-(player.effecttimeFive + (2 * time)));
		return _Utils_update(
			player,
			{effecttimeFive: newEffectTimeFive, effecttimeFour: newEffectTimeFour, effecttimeOne: newEffectTimeOne, effecttimeThree: newEffectTimeThree, effecttimeTwo: newEffectTimeTwo});
	});
var $author$project$Animate$chargeModeltime = function (model) {
	var newtime = model.time + 17;
	return _Utils_update(
		model,
		{time: newtime});
};
var $author$project$Text$cleartext = function (player) {
	return (player.textframe >= 100) ? _Utils_update(
		player,
		{text: '', textframe: 0}) : _Utils_update(
		player,
		{text: player.text});
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $author$project$Animate$count = F2(
	function (player, npc) {
		var newNPCCount = npc.count + 1;
		return (_Utils_eq(player.anim, $author$project$Model$Grovel) && (($elm$core$Basics$abs(player.pos.y1 - npc.pos.y1) < 400) && _Utils_eq(npc.count + 1, player.fallcount))) ? _Utils_update(
			npc,
			{count: newNPCCount}) : npc;
	});
var $author$project$Animate$health = function (player) {
	var hp = (player.hp >= 10) ? 10 : (player.hp + 0.0013);
	return _Utils_update(
		player,
		{hp: hp});
};
var $author$project$AnimState$charge = function (player) {
	return _Utils_update(
		player,
		{anim: $author$project$Model$Charge, frame: 0});
};
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$AnimState$walk = F2(
	function (moveDirection, player) {
		if (_Utils_eq(player.anim, $author$project$Model$Stand)) {
			if (moveDirection.$ === 'Left') {
				return _Utils_update(
					player,
					{
						anim: $author$project$Model$Walk,
						direction: moveDirection,
						jumpdir: $author$project$Message$L,
						speed: A2($author$project$Model$Vector, -0.2, 0)
					});
			} else {
				return _Utils_update(
					player,
					{
						anim: $author$project$Model$Walk,
						direction: moveDirection,
						jumpdir: $author$project$Message$R,
						speed: A2($author$project$Model$Vector, 0.2, 0)
					});
			}
		} else {
			return player;
		}
	});
var $author$project$Animate$moveSpeedAI = F2(
	function (time, speedAI) {
		var _v0 = $elm$core$List$head(speedAI.speedAIAnimList);
		if (_v0.$ === 'Just') {
			var anim = _v0.a;
			if (_Utils_cmp(anim.time, time) > 0) {
				return speedAI;
			} else {
				var speedAIAnimList = A2($elm$core$List$drop, 1, speedAI.speedAIAnimList);
				var newSpeedAI = _Utils_update(
					speedAI,
					{speedAIAnimList: speedAIAnimList});
				var _v1 = anim.msg;
				if (_v1.$ === 'AIWalk') {
					var moveDirection = _v1.a;
					var on = _v1.b;
					return on ? A2($author$project$AnimState$walk, moveDirection, newSpeedAI) : $author$project$AnimState$stand(newSpeedAI);
				} else {
					var jumpdir = _v1.a;
					var on = _v1.b;
					return on ? $author$project$AnimState$charge(newSpeedAI) : $author$project$AnimState$jump(
						_Utils_update(
							newSpeedAI,
							{jumpdir: jumpdir}));
				}
			}
		} else {
			return speedAI;
		}
	});
var $author$project$Animate$rage = function (player) {
	var ragecount = player.ragecount + 1;
	return ((player.hp <= 0) && (!_Utils_eq(player.mood, $author$project$Model$Rage))) ? _Utils_update(
		player,
		{mood: $author$project$Model$Rage, ragecount: ragecount}) : player;
};
var $author$project$Animate$touchDownBrick = F4(
	function (brickSpeed, time, brickPos, player) {
		if (A4($author$project$Collision$upImpact, brickSpeed, time, player.collisionPos, brickPos)) {
			var speed = A2($author$project$Model$Vector, player.speed.x, 0);
			var newplayer = A2($author$project$Animate$loseBlood, 1, player);
			var fallcount = player.fallcount + 1;
			var dy = (brickPos.y1 - (time * brickSpeed.y)) - player.pos.y2;
			var pos = A4($author$project$MapSetting$Pos, player.pos.x1, player.pos.x2, player.pos.y1 + dy, player.pos.y2 + dy);
			var collisionPos = $author$project$Model$standcollisionPos(pos);
			return _Utils_eq(player.anim, $author$project$Model$Jump) ? ((player.speed.y >= 1.8) ? $author$project$AnimState$grovel(
				_Utils_update(
					newplayer,
					{collisionPos: collisionPos, fallcount: fallcount, pos: pos})) : $author$project$AnimState$stand(
				_Utils_update(
					player,
					{collisionPos: collisionPos, pos: pos}))) : _Utils_update(
				player,
				{collisionPos: collisionPos, pos: pos, speed: speed});
		} else {
			return player;
		}
	});
var $author$project$Animate$touchDown = F3(
	function (time, bricks, player) {
		var brickSpeed = A2($author$project$Model$Vector, -player.speed.x, -player.speed.y);
		var posList = A2(
			$elm$core$List$map,
			A2($author$project$Animate$nextPos, brickSpeed, time),
			A2(
				$elm$core$List$map,
				function ($) {
					return $.pos;
				},
				bricks));
		return A3(
			$elm$core$List$foldl,
			A2($author$project$Animate$touchDownBrick, brickSpeed, time),
			player,
			posList);
	});
var $author$project$Animate$turn = function (character) {
	var _v0 = _Utils_eq(character.direction, $author$project$Message$Left) ? _Utils_Tuple2(
		$author$project$Message$Right,
		A2($author$project$Model$Vector, 0.05, 0)) : _Utils_Tuple2(
		$author$project$Message$Left,
		A2($author$project$Model$Vector, -0.05, 0));
	var direction = _v0.a;
	var speed = _v0.b;
	return _Utils_update(
		character,
		{anim: $author$project$Model$Walk, direction: direction, speed: speed});
};
var $author$project$Animate$tour = F2(
	function (time, character) {
		var pos = A3($author$project$Animate$nextPos, character.speed, time, character.pos);
		return ((_Utils_cmp(character.range.x, pos.x1) < 0) && (_Utils_cmp(pos.x2, character.range.y) < 0)) ? ((_Utils_eq(character.anim, $author$project$Model$Stand) && (character.frame >= 200)) ? $author$project$Animate$turn(character) : character) : $author$project$AnimState$stand(character);
	});
var $author$project$Animate$animate = F2(
	function (time, model) {
		var story = A2($author$project$Text$changeStory, model.state, model.story);
		var speedAI = _Utils_eq(model.state, $author$project$Model$Two) ? A2(
			$author$project$Animate$changeFrame,
			17,
			A2(
				$author$project$Animate$changePos,
				17,
				A3(
					$author$project$Animate$touchDown,
					17,
					_Utils_ap(model.map.bricks, model.map.wallbricks),
					A3(
						$author$project$Animate$changeSpeed,
						17,
						_Utils_ap(model.map.bricks, model.map.wallbricks),
						A3(
							$author$project$Animate$changeAnim,
							_Utils_ap(model.map.bricks, model.map.wallbricks),
							17,
							A2(
								$author$project$Animate$changeChargeTime,
								17,
								A2($author$project$Animate$moveSpeedAI, model.time, model.speedAI))))))) : model.speedAI;
		var player = _Utils_eq(model.player.anim, $author$project$Model$DebugMode) ? A2($author$project$Animate$changePos, time, model.player) : A2(
			$author$project$Animate$changeFrame,
			time,
			A2(
				$author$project$Animate$changePos,
				time,
				$author$project$Text$cleartext(
					A3(
						$author$project$Text$changeText,
						model.state,
						model.speedAI,
						A2(
							$author$project$Animate$changeTextframe,
							time,
							A3(
								$author$project$Animate$touchDown,
								time,
								_Utils_ap(model.map.bricks, model.map.wallbricks),
								A3(
									$author$project$Animate$changeSpeed,
									time,
									_Utils_ap(model.map.bricks, model.map.wallbricks),
									A3(
										$author$project$Animate$changeAnim,
										_Utils_ap(model.map.bricks, model.map.wallbricks),
										time,
										A2(
											$author$project$Animate$changeChargeTime,
											time,
											A2(
												$author$project$Animate$attackedByCharacters,
												model.map.characters,
												A2(
													$author$project$Animate$changeRageTime,
													time,
													A2(
														$author$project$Animate$chargeEffectTime,
														time,
														$author$project$Animate$health(
															$author$project$Animate$rage(model.player))))))))))))));
		var npcs = A2(
			$elm$core$List$map,
			function (npc) {
				return A2(
					$author$project$Text$changeNPCText,
					model,
					A2(
						$author$project$Animate$changeTextframe,
						time,
						A2($author$project$Animate$count, model.player, npc)));
			},
			model.map.npcs);
		var characters = A2(
			$elm$core$List$map,
			function (character) {
				return A2(
					$author$project$Animate$changeFrame,
					time,
					A2(
						$author$project$Animate$changePos,
						time,
						A2(
							$author$project$Animate$tour,
							time,
							A3(
								$author$project$Animate$changeAnim,
								_Utils_ap(model.map.bricks, model.map.wallbricks),
								time,
								A2($author$project$Animate$attackPlayer, model.player, character)))));
			},
			A2(
				$elm$core$List$filter,
				function (character) {
					return !A2($author$project$Animate$attackedByPlayer, player, character);
				},
				model.map.characters));
		var map = A3($author$project$Animate$changeCharactersAndNpcs, characters, npcs, model.map);
		return A2(
			$author$project$ChangeState$changeCGandStory,
			time,
			$author$project$ChangeState$changeState(
				$author$project$Animate$chargeModeltime(
					_Utils_update(
						model,
						{map: map, player: player, speedAI: speedAI, story: story}))));
	});
var $author$project$Update$setDebugSpeed = F2(
	function (speed, player) {
		return _Utils_update(
			player,
			{anim: $author$project$Model$DebugMode, speed: speed});
	});
var $author$project$Update$changeDebugSpeed = F3(
	function (speed, on, player) {
		return on ? A2($author$project$Update$setDebugSpeed, speed, player) : A2(
			$author$project$Update$setDebugSpeed,
			A2($author$project$Model$Vector, 0, 0),
			player);
	});
var $author$project$AnimState$jumpdirection = F2(
	function (moveDirection, player) {
		if (moveDirection.$ === 'Left') {
			return _Utils_update(
				player,
				{jumpdir: $author$project$Message$L});
		} else {
			return _Utils_update(
				player,
				{jumpdir: $author$project$Message$R});
		}
	});
var $elm$core$Debug$log = _Debug_log;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$Basics$not = _Basics_not;
var $author$project$Update$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'GetViewport':
				var viewport = msg.a.viewport;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							size: A2($author$project$Model$Vector, viewport.width, viewport.height)
						}),
					$elm$core$Platform$Cmd$none);
			case 'ImageLoaded':
				var url = msg.a;
				var loadPack = A2(
					$elm$core$List$filter,
					function (loadingurl) {
						return !_Utils_eq(loadingurl, url);
					},
					model.loadPack);
				return $elm$core$List$isEmpty(loadPack) ? _Utils_Tuple2(
					_Utils_update(
						model,
						{state: $author$project$Model$LOGO}),
					$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					_Utils_update(
						model,
						{loadPack: loadPack}),
					$elm$core$Platform$Cmd$none);
			case 'ImageError':
				var url = msg.a;
				var loadPack = A2(
					$elm$core$List$filter,
					function (loadingurl) {
						return !_Utils_eq(loadingurl, url);
					},
					model.loadPack);
				return $elm$core$List$isEmpty(loadPack) ? _Utils_Tuple2(
					_Utils_update(
						model,
						{state: $author$project$Model$LOGO}),
					$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					_Utils_update(
						model,
						{loadPack: loadPack}),
					$elm$core$Platform$Cmd$none);
			case 'Resize':
				var width = msg.a;
				var height = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							size: A2($author$project$Model$Vector, width, height)
						}),
					$elm$core$Platform$Cmd$none);
			case 'AnimWalk':
				var moveDirection = msg.a;
				var on = msg.b;
				return (on && _Utils_eq(model.player.anim, $author$project$Model$Stand)) ? _Utils_Tuple2(
					_Utils_update(
						model,
						{
							player: A2($author$project$AnimState$walk, moveDirection, model.player),
							record: _Utils_ap(
								model.record,
								_List_fromArray(
									[
										A2(
										$elm$core$Debug$log,
										'msg',
										{
											msg: A2($author$project$AISettings$AIWalk, moveDirection, on),
											time: model.time
										})
									]))
						}),
					$elm$core$Platform$Cmd$none) : ((on && _Utils_eq(model.player.anim, $author$project$Model$Charge)) ? _Utils_Tuple2(
					_Utils_update(
						model,
						{
							player: A2($author$project$AnimState$jumpdirection, moveDirection, model.player)
						}),
					$elm$core$Platform$Cmd$none) : (((!on) && ((!_Utils_eq(model.player.anim, $author$project$Model$Jump)) && (!model.player.speed.y))) ? _Utils_Tuple2(
					_Utils_update(
						model,
						{
							player: $author$project$AnimState$stand(model.player),
							record: _Utils_ap(
								model.record,
								_List_fromArray(
									[
										A2(
										$elm$core$Debug$log,
										'msg',
										{
											msg: A2($author$project$AISettings$AIWalk, moveDirection, on),
											time: model.time
										})
									]))
						}),
					$elm$core$Platform$Cmd$none) : _Utils_Tuple2(model, $elm$core$Platform$Cmd$none)));
			case 'AnimCharge':
				var on = msg.a;
				return (on && _Utils_eq(model.player.anim, $author$project$Model$Stand)) ? _Utils_Tuple2(
					_Utils_update(
						model,
						{
							player: $author$project$AnimState$charge(model.player),
							record: _Utils_ap(
								model.record,
								_List_fromArray(
									[
										A2(
										$elm$core$Debug$log,
										'msg',
										{
											msg: A2($author$project$AISettings$AICharge, model.player.jumpdir, on),
											time: model.time
										})
									]))
						}),
					$elm$core$Platform$Cmd$none) : (((!on) && _Utils_eq(model.player.anim, $author$project$Model$Charge)) ? _Utils_Tuple2(
					_Utils_update(
						model,
						{
							player: $author$project$AnimState$jump(model.player),
							record: _Utils_ap(
								model.record,
								_List_fromArray(
									[
										A2(
										$elm$core$Debug$log,
										'msg',
										{
											msg: A2($author$project$AISettings$AICharge, model.player.jumpdir, on),
											time: model.time
										})
									]))
						}),
					$elm$core$Platform$Cmd$none) : _Utils_Tuple2(model, $elm$core$Platform$Cmd$none));
			case 'Tick':
				var time = msg.a;
				return _Utils_Tuple2(
					A2($author$project$Animate$animate, time, model),
					$elm$core$Platform$Cmd$none);
			case 'AnimAttack':
				var on = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							player: $author$project$AnimState$attack(model.player)
						}),
					$elm$core$Platform$Cmd$none);
			case 'DebugRight':
				var on = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							player: A3(
								$author$project$Update$changeDebugSpeed,
								A2($author$project$Model$Vector, 0.2, 0),
								on,
								model.player)
						}),
					$elm$core$Platform$Cmd$none);
			case 'DebugLeft':
				var on = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							player: A3(
								$author$project$Update$changeDebugSpeed,
								A2($author$project$Model$Vector, -0.2, 0),
								on,
								model.player)
						}),
					$elm$core$Platform$Cmd$none);
			case 'DebugUp':
				var on = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							player: A3(
								$author$project$Update$changeDebugSpeed,
								A2($author$project$Model$Vector, 0, -0.2),
								on,
								model.player)
						}),
					$elm$core$Platform$Cmd$none);
			case 'DebugDown':
				var on = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							player: A3(
								$author$project$Update$changeDebugSpeed,
								A2($author$project$Model$Vector, 0, 0.2),
								on,
								model.player)
						}),
					$elm$core$Platform$Cmd$none);
			case 'ExitDebugMode':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							player: $author$project$AnimState$stand(model.player)
						}),
					$elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Model$Loading = {$: 'Loading'};
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $author$project$View$viewAttrs = {
	size: A2($author$project$Model$Vector, 1600, 800)
};
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $author$project$View$gameUIAttribute = function (size) {
	return _List_fromArray(
		[
			$elm$svg$Svg$Attributes$width(
			$elm$core$String$fromFloat(size.x)),
			$elm$svg$Svg$Attributes$height(
			$elm$core$String$fromFloat(size.y)),
			$elm$svg$Svg$Attributes$viewBox(
			'0 0 ' + ($elm$core$String$fromFloat($author$project$View$viewAttrs.size.x) + (' ' + $elm$core$String$fromFloat($author$project$View$viewAttrs.size.y))))
		]);
};
var $author$project$Message$ImageError = function (a) {
	return {$: 'ImageError', a: a};
};
var $author$project$Message$ImageLoaded = function (a) {
	return {$: 'ImageLoaded', a: a};
};
var $elm$html$Html$img = _VirtualDom_node('img');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$src = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var $author$project$Load$loadImg = function (url) {
	return A2(
		$elm$html$Html$img,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$src(url),
				A2(
				$elm$html$Html$Events$on,
				'error',
				$elm$json$Json$Decode$succeed(
					$author$project$Message$ImageError(url))),
				A2(
				$elm$html$Html$Events$on,
				'load',
				$elm$json$Json$Decode$succeed(
					$author$project$Message$ImageLoaded(url)))
			]),
		_List_Nil);
};
var $elm$svg$Svg$Attributes$opacity = _VirtualDom_attribute('opacity');
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$image = $elm$svg$Svg$trustedNode('image');
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$xlinkHref = function (value) {
	return A3(
		_VirtualDom_attributeNS,
		'http://www.w3.org/1999/xlink',
		'xlink:href',
		_VirtualDom_noJavaScriptUri(value));
};
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $author$project$View$renderImage = F3(
	function (url, pos, attr) {
		return A2(
			$elm$svg$Svg$image,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$xlinkHref(url),
						$elm$svg$Svg$Attributes$width(
						$elm$core$String$fromFloat(pos.x2 - pos.x1)),
						$elm$svg$Svg$Attributes$height(
						$elm$core$String$fromFloat(pos.y2 - pos.y1)),
						A2(
						$elm$core$List$member,
						$elm$svg$Svg$Attributes$transform('scale (-1 1)'),
						attr) ? $elm$svg$Svg$Attributes$x(
						$elm$core$String$fromFloat(-pos.x2)) : $elm$svg$Svg$Attributes$x(
						$elm$core$String$fromFloat(pos.x1)),
						$elm$svg$Svg$Attributes$y(
						$elm$core$String$fromFloat(pos.y1))
					]),
				attr),
			_List_Nil);
	});
var $author$project$View$renderBackground = function (model) {
	return _Utils_eq(model.state, $author$project$Model$LOGO) ? A3(
		$author$project$View$renderImage,
		'img/background.png',
		A4($author$project$MapSetting$Pos, 0, $author$project$View$viewAttrs.size.x, 0, $author$project$View$viewAttrs.size.y),
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$opacity(
				$elm$core$String$fromFloat(
					1 - (A2($elm$core$Basics$pow, (model.cgtime / 1000) - 2.5, 4) / 40)))
			])) : (_Utils_eq(model.state, $author$project$Model$One) ? (((model.player.pos.y1 >= 3200) || ((model.player.pos.y1 <= 1600) && (model.player.pos.x1 >= 3200))) ? A3(
		$author$project$View$renderImage,
		'img/background/background1_2.png',
		A4($author$project$MapSetting$Pos, 0, $author$project$View$viewAttrs.size.x, 0, $author$project$View$viewAttrs.size.y),
		_List_Nil) : A3(
		$author$project$View$renderImage,
		'img/background/background1_1.png',
		A4($author$project$MapSetting$Pos, 0, $author$project$View$viewAttrs.size.x, 0, $author$project$View$viewAttrs.size.y),
		_List_Nil)) : (_Utils_eq(model.state, $author$project$Model$DiscoverI) ? ((model.player.pos.y1 >= 3200) ? A3(
		$author$project$View$renderImage,
		'img/background/background2_2.png',
		A4($author$project$MapSetting$Pos, 0, $author$project$View$viewAttrs.size.x, 0, $author$project$View$viewAttrs.size.y),
		_List_Nil) : A3(
		$author$project$View$renderImage,
		'img/background/background2_1.png',
		A4($author$project$MapSetting$Pos, 0, $author$project$View$viewAttrs.size.x, 0, $author$project$View$viewAttrs.size.y),
		_List_Nil)) : (_Utils_eq(model.state, $author$project$Model$Two) ? A3(
		$author$project$View$renderImage,
		'img/background/background3_1.png',
		A4($author$project$MapSetting$Pos, 0, $author$project$View$viewAttrs.size.x, 0, $author$project$View$viewAttrs.size.y),
		_List_Nil) : (_Utils_eq(model.state, $author$project$Model$DiscoverII) ? A3(
		$author$project$View$renderImage,
		'img/background/background4_1.png',
		A4($author$project$MapSetting$Pos, 0, $author$project$View$viewAttrs.size.x, 0, $author$project$View$viewAttrs.size.y),
		_List_Nil) : (_Utils_eq(model.state, $author$project$Model$Three) ? A3(
		$author$project$View$renderImage,
		'img/background/background5_1.png',
		A4($author$project$MapSetting$Pos, 0, $author$project$View$viewAttrs.size.x, 0, $author$project$View$viewAttrs.size.y),
		_List_Nil) : A3(
		$author$project$View$renderImage,
		'img/background.png',
		A4($author$project$MapSetting$Pos, 0, $author$project$View$viewAttrs.size.x, 0, $author$project$View$viewAttrs.size.y),
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$opacity('0')
			])))))));
};
var $author$project$View$renderBlood = function (player) {
	return _Utils_eq(player.mood, $author$project$Model$Normal) ? (((player.hp >= 7) && (player.hp < 10)) ? _List_fromArray(
		[
			A3(
			$author$project$View$renderImage,
			'img/Effect/bloodFrame_1.png',
			A4($author$project$MapSetting$Pos, 0, 1600, 0, 800),
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(
						$elm$core$Basics$abs(player.effecttimeOne) / 2000))
				]))
		]) : (((player.hp >= 5) && (player.hp < 7)) ? _List_fromArray(
		[
			A3(
			$author$project$View$renderImage,
			'img/Effect/bloodFrame_1.png',
			A4($author$project$MapSetting$Pos, 0, 1600, 0, 800),
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(
						$elm$core$Basics$abs(player.effecttimeTwo) / 1750))
				])),
			A3(
			$author$project$View$renderImage,
			'img/Effect/bloodFrame_2.png',
			A4($author$project$MapSetting$Pos, 0, 1600, 0, 800),
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(
						$elm$core$Basics$abs(player.effecttimeOne) / 1750))
				]))
		]) : (((player.hp >= 3) && (player.hp < 5)) ? _List_fromArray(
		[
			A3(
			$author$project$View$renderImage,
			'img/Effect/bloodFrame_1.png',
			A4($author$project$MapSetting$Pos, 0, 1600, 0, 800),
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(
						$elm$core$Basics$abs(player.effecttimeThree) / 1500))
				])),
			A3(
			$author$project$View$renderImage,
			'img/Effect/bloodFrame_2.png',
			A4($author$project$MapSetting$Pos, 0, 1600, 0, 800),
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(
						$elm$core$Basics$abs(player.effecttimeTwo) / 1500))
				])),
			A3(
			$author$project$View$renderImage,
			'img/Effect/bloodFrame_3.png',
			A4($author$project$MapSetting$Pos, 0, 1600, 0, 800),
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(
						$elm$core$Basics$abs(player.effecttimeThree) / 1500))
				]))
		]) : (((player.hp > 0) && (player.hp < 3)) ? _List_fromArray(
		[
			A3(
			$author$project$View$renderImage,
			'img/Effect/bloodFrame_1.png',
			A4($author$project$MapSetting$Pos, 0, 1600, 0, 800),
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(
						$elm$core$Basics$abs(player.effecttimeFour) / 1250))
				])),
			A3(
			$author$project$View$renderImage,
			'img/Effect/bloodFrame_2.png',
			A4($author$project$MapSetting$Pos, 0, 1600, 0, 800),
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(
						$elm$core$Basics$abs(player.effecttimeTwo) / 1250))
				])),
			A3(
			$author$project$View$renderImage,
			'img/Effect/bloodFrame_3.png',
			A4($author$project$MapSetting$Pos, 0, 1600, 0, 800),
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(
						$elm$core$Basics$abs(player.effecttimeThree) / 1250))
				])),
			A3(
			$author$project$View$renderImage,
			'img/Effect/bloodFrame_4.png',
			A4($author$project$MapSetting$Pos, 0, 1600, 0, 800),
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(
						$elm$core$Basics$abs(player.effecttimeFour) / 1250))
				]))
		]) : _List_fromArray(
		[
			A3(
			$author$project$View$renderImage,
			'img/Effect/bloodFrame_1.png',
			A4($author$project$MapSetting$Pos, 0, 0, 0, 0),
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity('0')
				]))
		]))))) : _List_fromArray(
		[
			A3(
			$author$project$View$renderImage,
			'img/Effect/bloodFrame_1.png',
			A4($author$project$MapSetting$Pos, 0, 1600, 0, 800),
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(
						$elm$core$Basics$abs(player.effecttimeFive) / 1000))
				])),
			A3(
			$author$project$View$renderImage,
			'img/Effect/bloodFrame_2.png',
			A4($author$project$MapSetting$Pos, 0, 1600, 0, 800),
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(
						$elm$core$Basics$abs(player.effecttimeThree) / 1000))
				])),
			A3(
			$author$project$View$renderImage,
			'img/Effect/bloodFrame_3.png',
			A4($author$project$MapSetting$Pos, 0, 1600, 0, 800),
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(
						$elm$core$Basics$abs(player.effecttimeFour) / 1000))
				])),
			A3(
			$author$project$View$renderImage,
			'img/Effect/bloodFrame_4.png',
			A4($author$project$MapSetting$Pos, 0, 1600, 0, 800),
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(
						$elm$core$Basics$abs(player.effecttimeFive) / 1000))
				])),
			A3(
			$author$project$View$renderImage,
			'img/Effect/bloodFrame_5.png',
			A4($author$project$MapSetting$Pos, 0, 1600, 0, 800),
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(
						$elm$core$Basics$abs(player.effecttimeFive) / 1000))
				]))
		]);
};
var $elm$core$Debug$toString = _Debug_toString;
var $author$project$View$renderCG = function (model) {
	return A3(
		$author$project$View$renderImage,
		'img/CG/' + ($elm$core$Debug$toString(model.state) + '.png'),
		A4($author$project$MapSetting$Pos, 0, $author$project$View$viewAttrs.size.x, 0, $author$project$View$viewAttrs.size.y),
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$opacity(
				$elm$core$String$fromFloat(
					1 - (A2($elm$core$Basics$pow, (model.cgtime / 1000) - 2.5, 4) / 40)))
			]));
};
var $author$project$View$clearOutsideImage = function (viewpos) {
	return ((viewpos.x2 < 0) || ((_Utils_cmp(viewpos.x1, $author$project$View$viewAttrs.size.x) > 0) || ((viewpos.y2 < 0) || (_Utils_cmp(viewpos.y1, $author$project$View$viewAttrs.size.y) > 0)))) ? A4($author$project$MapSetting$Pos, -1600, -1600, 0, 0) : viewpos;
};
var $author$project$View$connectName = F3(
	function (namePrefix, anim, id) {
		return namePrefix + (anim + ('/' + (namePrefix + (anim + ('_' + A3(
			$elm$core$String$padLeft,
			4,
			_Utils_chr('0'),
			$elm$core$String$fromInt(id)))))));
	});
var $elm$core$Basics$modBy = _Basics_modBy;
var $author$project$View$getAnimUrl = F4(
	function (anim, frame, player, namePrefix) {
		var surfix = '.png';
		var prefix = 'img/character/color/';
		var name = function () {
			switch (anim.$) {
				case 'Stand':
					return A3($author$project$View$connectName, namePrefix, 'walk', 0);
				case 'Walk':
					var id = (namePrefix === '') ? A2($elm$core$Basics$modBy, 65, frame) : A2($elm$core$Basics$modBy, 60, frame);
					return A3($author$project$View$connectName, namePrefix, 'walk', id);
				case 'Charge':
					var id = (frame < 50) ? 1 : ((frame < 120) ? 0 : 2);
					return A3($author$project$View$connectName, namePrefix, 'charge', id);
				case 'Jump':
					return A3($author$project$View$connectName, namePrefix, 'jump', 0);
				case 'Attack':
					var id = A2($elm$core$Basics$modBy, 60, frame);
					return A3($author$project$View$connectName, namePrefix, 'attack', id);
				case 'Crouch':
					return A3($author$project$View$connectName, namePrefix, 'charge', 2);
				case 'Grovel':
					return A3($author$project$View$connectName, namePrefix, 'grovel', 0);
				case 'Attacked':
					return (((player.speed.x < 0) && _Utils_eq(player.direction, $author$project$Message$Left)) || ((player.speed.x > 0) && _Utils_eq(player.direction, $author$project$Message$Right))) ? (namePrefix + ('attacked/' + (namePrefix + 'attackedBack_0000'))) : (namePrefix + ('attacked/' + (namePrefix + 'attackedFront_0000')));
				case 'Fly':
					return A3($author$project$View$connectName, namePrefix, 'jump', 0);
				default:
					return A3($author$project$View$connectName, namePrefix, 'walk', 0);
			}
		}();
		return _Utils_ap(
			prefix,
			_Utils_ap(name, surfix));
	});
var $author$project$View$getDirectionAttr = function (direction) {
	if (direction.$ === 'Left') {
		return _List_fromArray(
			[
				$elm$svg$Svg$Attributes$transform('scale (-1 1)')
			]);
	} else {
		return _List_Nil;
	}
};
var $author$project$View$offset = F2(
	function (player, pos) {
		var dy = $elm$core$Basics$floor(player.pos.y1 / $author$project$View$viewAttrs.size.y) * $author$project$View$viewAttrs.size.y;
		var dx = $elm$core$Basics$floor(player.pos.x1 / $author$project$View$viewAttrs.size.x) * $author$project$View$viewAttrs.size.x;
		return A4($author$project$MapSetting$Pos, pos.x1 - dx, pos.x2 - dx, pos.y1 - dy, pos.y2 - dy);
	});
var $author$project$View$resizePlayer = function (pos) {
	var y2 = pos.y2;
	var y = (768 / 700) * (pos.y2 - pos.y1);
	var y1 = y2 - y;
	var x = (1366 / 455) * (pos.x2 - pos.x1);
	var dx = (x - (pos.x2 - pos.x1)) / 2;
	var x1 = pos.x1 - dx;
	var x2 = pos.x2 + dx;
	return A4($author$project$MapSetting$Pos, x1, x2, y1, y2);
};
var $author$project$View$renderCharacter = F2(
	function (player, character) {
		var viewpos = $author$project$View$clearOutsideImage(
			$author$project$View$resizePlayer(
				A2($author$project$View$offset, player, character.pos)));
		var url = A4($author$project$View$getAnimUrl, character.anim, character.frame, character, 'character_');
		var attr = $author$project$View$getDirectionAttr(character.direction);
		return A3($author$project$View$renderImage, url, viewpos, attr);
	});
var $author$project$View$renderCharacters = F2(
	function (player, characters) {
		return A2(
			$elm$core$List$map,
			$author$project$View$renderCharacter(player),
			characters);
	});
var $author$project$View$renderLOGO = function (model) {
	return A3(
		$author$project$View$renderImage,
		'img/LOGO.png',
		A4($author$project$MapSetting$Pos, 0, $author$project$View$viewAttrs.size.x, 0, $author$project$View$viewAttrs.size.y),
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$opacity(
				$elm$core$String$fromFloat(
					1 - (A2($elm$core$Basics$pow, (model.cgtime / 1000) - 2.5, 4) / 40)))
			]));
};
var $author$project$View$renderNPC = F2(
	function (player, npc) {
		var url = A4($author$project$View$getAnimUrl, npc.anim, npc.frame, npc, 'NPC_');
		var pos = $author$project$View$clearOutsideImage(
			A2($author$project$View$offset, player, npc.pos));
		var attr = $author$project$View$getDirectionAttr(npc.direction);
		return A3(
			$author$project$View$renderImage,
			url,
			A4($author$project$MapSetting$Pos, pos.x1, pos.x2, pos.y1 + 10, pos.y2 + 10),
			attr);
	});
var $author$project$View$renderNPCs = F2(
	function (player, npcs) {
		return A2(
			$elm$core$List$map,
			$author$project$View$renderNPC(player),
			npcs);
	});
var $elm$svg$Svg$foreignObject = $elm$svg$Svg$trustedNode('foreignObject');
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$svg$Svg$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$View$renderT = F5(
	function (size, pos, w, lines, text) {
		return A2(
			$elm$svg$Svg$foreignObject,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x(
					$elm$core$String$fromFloat(pos.x1 + 70)),
					$elm$svg$Svg$Attributes$y(
					$elm$core$String$fromFloat(pos.y1 - (lines * 20))),
					$elm$svg$Svg$Attributes$width(
					$elm$core$String$fromFloat(w)),
					$elm$svg$Svg$Attributes$height(
					$elm$core$String$fromFloat(lines * 22))
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$Attributes$style,
							'font-size',
							$elm$core$String$fromFloat(size) + 'px')
						]),
					_List_fromArray(
						[
							$elm$svg$Svg$text(text)
						]))
				]));
	});
var $author$project$View$renderNPCText = F2(
	function (player, npc) {
		var w = 180;
		var size = 18;
		var pos = $author$project$View$clearOutsideImage(
			A2(
				$author$project$View$offset,
				player,
				A4($author$project$MapSetting$Pos, npc.pos.x1 - 150, npc.pos.x2 - 150, npc.pos.y1 + 10, npc.pos.y2 + 10)));
		var lines = $elm$core$Basics$floor(
			$elm$core$String$length(npc.text) / 20) + 2;
		return A5($author$project$View$renderT, size, pos, w, lines, npc.text);
	});
var $author$project$View$renderNPCsText = F2(
	function (player, npcs) {
		return A2(
			$elm$core$List$map,
			$author$project$View$renderNPCText(player),
			npcs);
	});
var $author$project$View$getPlayerViewPos = function (player) {
	return $author$project$View$resizePlayer(
		A2($author$project$View$offset, player, player.pos));
};
var $author$project$View$renderPlayer = F2(
	function (state, player) {
		var url = A4($author$project$View$getAnimUrl, player.anim, player.frame, player, '');
		var pos = $author$project$View$getPlayerViewPos(player);
		var attr = _Utils_eq(state, $author$project$Model$Two) ? _Utils_ap(
			$author$project$View$getDirectionAttr(player.direction),
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$opacity('0.5')
				])) : $author$project$View$getDirectionAttr(player.direction);
		return _Utils_eq(player.anim, $author$project$Model$Grovel) ? A3(
			$author$project$View$renderImage,
			url,
			A4($author$project$MapSetting$Pos, pos.x1 + 85, pos.x2 - 85, pos.y1 + 30, pos.y2 + 30),
			attr) : A3($author$project$View$renderImage, url, pos, attr);
	});
var $author$project$View$renderPlayerText = function (player) {
	var w = 180;
	var text = player.text;
	var size = 18;
	var pos = $author$project$View$getPlayerViewPos(player);
	var lines = $elm$core$Basics$floor(
		$elm$core$String$length(player.text) / 20) + 3;
	return A5($author$project$View$renderT, size, pos, w, lines, text);
};
var $author$project$View$renderS = F4(
	function (size, w, lines, text) {
		return A2(
			$elm$svg$Svg$foreignObject,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x('200'),
					$elm$svg$Svg$Attributes$y('80'),
					$elm$svg$Svg$Attributes$width(
					$elm$core$String$fromFloat(w)),
					$elm$svg$Svg$Attributes$height(
					$elm$core$String$fromFloat(lines * 20))
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$Attributes$style,
							'font-size',
							$elm$core$String$fromFloat(size) + 'px')
						]),
					_List_fromArray(
						[
							$elm$svg$Svg$text(text)
						]))
				]));
	});
var $author$project$View$renderSpeedAI = F2(
	function (player, speedAI) {
		var viewpos = $author$project$View$clearOutsideImage(
			$author$project$View$resizePlayer(
				A2($author$project$View$offset, player, speedAI.pos)));
		var url = A4($author$project$View$getAnimUrl, speedAI.anim, speedAI.frame, speedAI, '');
		var attr = $author$project$View$getDirectionAttr(speedAI.direction);
		return _List_fromArray(
			[
				A3($author$project$View$renderImage, url, viewpos, attr)
			]);
	});
var $author$project$View$renderStory = function (story) {
	var w = 1000;
	var text = story.text;
	var size = 20;
	var lines = $elm$core$Basics$floor(
		$elm$core$String$length(story.text) / 20) + 2;
	return A4($author$project$View$renderS, size, w, lines, text);
};
var $author$project$View$cutBrickView = function (viewpos) {
	var y2 = (_Utils_cmp(viewpos.y2, $author$project$View$viewAttrs.size.y) > 0) ? $author$project$View$viewAttrs.size.y : viewpos.y2;
	var y1 = (viewpos.y1 < 0) ? 0 : viewpos.y1;
	var x2 = (_Utils_cmp(viewpos.x2, $author$project$View$viewAttrs.size.x) > 0) ? $author$project$View$viewAttrs.size.x : viewpos.x2;
	var x1 = (viewpos.x1 < 0) ? 0 : viewpos.x1;
	return A4($author$project$MapSetting$Pos, x1, x2, y1, y2);
};
var $author$project$View$toHtmlPos = F2(
	function (size, pos) {
		var x2 = (pos.x2 / 1600) * size.x;
		var x1 = (pos.x1 / 1600) * size.x;
		var dy = (size.y / 2) - ((size.x / 2) / 2);
		var y1 = ((pos.y1 / 1600) * size.x) + dy;
		var y2 = ((pos.y2 / 1600) * size.x) + dy;
		return A4($author$project$MapSetting$Pos, x1, x2, y1, y2);
	});
var $author$project$View$renderHtmlImg = F3(
	function (size, url, svgpos) {
		var pos = A2($author$project$View$toHtmlPos, size, svgpos);
		return A2(
			$elm$html$Html$img,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$src(url),
					$elm$svg$Svg$Attributes$height(
					$elm$core$String$fromFloat(pos.y2 - pos.y1) + 'px'),
					$elm$svg$Svg$Attributes$width(
					$elm$core$String$fromFloat(pos.x2 - pos.x1) + 'px'),
					A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
					A2(
					$elm$html$Html$Attributes$style,
					'left',
					$elm$core$String$fromFloat(pos.x1) + 'px'),
					A2(
					$elm$html$Html$Attributes$style,
					'top',
					$elm$core$String$fromFloat(pos.y1) + 'px')
				]),
			_List_Nil);
	});
var $author$project$View$renderbrick1 = F2(
	function (model, pos) {
		var viewpos = $author$project$View$clearOutsideImage(
			$author$project$View$cutBrickView(
				A2($author$project$View$offset, model.player, pos)));
		var text = _Utils_eq(model.state, $author$project$Model$One) ? 'img/Stone/map_1/stone_1.png' : (_Utils_eq(model.state, $author$project$Model$DiscoverI) ? 'img/Stone/map_2/stone_1.png' : (_Utils_eq(model.state, $author$project$Model$Two) ? 'img/Stone/map_3/stone_1.png' : (_Utils_eq(model.state, $author$project$Model$DiscoverII) ? 'img/Stone/map_4/stone_1.png' : (_Utils_eq(model.state, $author$project$Model$Three) ? 'img/Stone/map_5/stone_1.png' : 'img/Stone/map_1/stone_1.png'))));
		return A3($author$project$View$renderHtmlImg, model.size, text, viewpos);
	});
var $author$project$View$renderbricks = F2(
	function (posList, model) {
		return A2(
			$elm$core$List$map,
			$author$project$View$renderbrick1(model),
			posList);
	});
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $author$project$View$view = function (model) {
	var renderSvg = A2(
		$elm$svg$Svg$svg,
		$author$project$View$gameUIAttribute(model.size),
		(_Utils_eq(model.state, $author$project$Model$Story1_1) || (_Utils_eq(model.state, $author$project$Model$Story1_2) || (_Utils_eq(model.state, $author$project$Model$Story1_3) || (_Utils_eq(model.state, $author$project$Model$Story1_4) || (_Utils_eq(model.state, $author$project$Model$Story2_1) || (_Utils_eq(model.state, $author$project$Model$Story2_2) || (_Utils_eq(model.state, $author$project$Model$Story3_1) || (_Utils_eq(model.state, $author$project$Model$Story4_1) || (_Utils_eq(model.state, $author$project$Model$Story5_1) || (_Utils_eq(model.state, $author$project$Model$Story5_2) || _Utils_eq(model.state, $author$project$Model$Story6_1))))))))))) ? _List_fromArray(
			[
				$author$project$View$renderStory(model.story)
			]) : ((_Utils_eq(model.state, $author$project$Model$CG1_1) || (_Utils_eq(model.state, $author$project$Model$CG1_2) || (_Utils_eq(model.state, $author$project$Model$CG1_3) || (_Utils_eq(model.state, $author$project$Model$CG1_4) || (_Utils_eq(model.state, $author$project$Model$CG2_1) || (_Utils_eq(model.state, $author$project$Model$CG2_2) || (_Utils_eq(model.state, $author$project$Model$CG3_1) || (_Utils_eq(model.state, $author$project$Model$CG5_1) || (_Utils_eq(model.state, $author$project$Model$CG5_2) || (_Utils_eq(model.state, $author$project$Model$CG6_1) || _Utils_eq(model.state, $author$project$Model$CG6_2))))))))))) ? _List_fromArray(
			[
				$author$project$View$renderCG(model)
			]) : (_Utils_eq(model.state, $author$project$Model$Loading) ? _List_fromArray(
			[
				A4($author$project$View$renderS, 20, 100, 1, 'Loading...')
			]) : (_Utils_eq(model.state, $author$project$Model$LOGO) ? _List_fromArray(
			[
				$author$project$View$renderLOGO(model),
				$author$project$View$renderBackground(model)
			]) : _Utils_ap(
			_List_fromArray(
				[
					$author$project$View$renderBackground(model),
					A2($author$project$View$renderPlayer, model.state, model.player)
				]),
			_Utils_ap(
				A2($author$project$View$renderCharacters, model.player, model.map.characters),
				_Utils_ap(
					A2($author$project$View$renderNPCs, model.player, model.map.npcs),
					_Utils_ap(
						_Utils_eq(model.state, $author$project$Model$Two) ? A2($author$project$View$renderSpeedAI, model.player, model.speedAI) : _List_Nil,
						_Utils_ap(
							_List_fromArray(
								[
									$author$project$View$renderPlayerText(model.player)
								]),
							_Utils_ap(
								A2($author$project$View$renderNPCsText, model.player, model.map.npcs),
								$author$project$View$renderBlood(model.player)))))))))));
	var renderLoad = _Utils_eq(model.state, $author$project$Model$Loading) ? A2($elm$core$List$map, $author$project$Load$loadImg, $author$project$Load$initLoadPack) : _List_Nil;
	var renderHtml = (_Utils_eq(model.state, $author$project$Model$One) || (_Utils_eq(model.state, $author$project$Model$DiscoverI) || (_Utils_eq(model.state, $author$project$Model$Two) || (_Utils_eq(model.state, $author$project$Model$DiscoverII) || _Utils_eq(model.state, $author$project$Model$Three))))) ? A2(
		$author$project$View$renderbricks,
		A2(
			$elm$core$List$map,
			function ($) {
				return $.pos;
			},
			model.map.bricks),
		model) : _List_Nil;
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'height', '0px')
					]),
				_List_fromArray(
					[
						renderSvg,
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'opacity', '0')
							]),
						renderLoad)
					])),
				A2($elm$html$Html$div, _List_Nil, renderHtml)
			]));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{init: $author$project$Model$init, subscriptions: $author$project$Main$subscriptions, update: $author$project$Update$update, view: $author$project$View$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));