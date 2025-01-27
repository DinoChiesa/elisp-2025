// example invocation:
//   jsonnet --ext-str-file applist=./data/gaambo-devs.json  map-array.jsonnet
//
// more:
// https://jsonnet.org/learning/tutorial.html

local applist = std.parseJson(std.extVar('applist'));
local dlist = std.parseJson(std.extVar('dlist'));

/* check for presence of property in JSON hash */
local apps = if 'apps' in applist then applist.apps else [];
local s = 'apple, pear, persimmon, banana';

{
  devapps: apps,
  /* length of array */
  numapps: std.length(apps),

  /* map a function on an array (d.developer) */
  devemails: std.map(function(d) d.email, dlist.developer),

  /* split a space-separated string */
  fruits: [std.stripChars(x, ' ') for x in std.split(s, ',')],

  /* refer to prior fields, and string formatting */
  first: 'The first fruit is %s.' % self.fruits[0],
}

/*
    Formatting Strings With the Modulo Operator (%)

    Strings in Python have a built-in operation that you can access with the
    modulo operator (%). This operator lets you do positional and named string
    interpolation. If you’ve ever worked with the printf() function in C, then
    the syntax will be familiar. Here’s a toy example:

    >>> name = "Bob"

    >>> "Hello, %s" % name
    'Hello, Bob'

    The %s substring is a conversion specifier that works as a replacement
    field. It tells Python where to substitute the value of name, represented as
    a string.

    Using Conversion Specifiers

    To build a conversion specifier, you need two or more characters. Here’s a
    quick summary of the accepted characters and their corresponding order in
    the specifier:

    The % character marks the start of the specifier.
    An optional mapping key in parentheses allows you to use named replacement fields like (name).
    An optional conversion flag affects how some conversion types display.
    An optional minimum field width allows you to define the number of characters to display.
    An optional precision consists of a dot character (.) followed by the desired precision.
    An optional length modifier is an l or h for long and short integers.
    A conversion type specifies how the output string will be formatted, mimicking different data types.

    Several conversion types are available for the modulo operator in
    Python. They allow you to control the output’s format in some way. For
    example, you can convert numbers to hexadecimal notation or add whitespace
    padding to generate nicely formatted tables and reports.

    Here’s a summary of the conversion types currently available in Python:

    Conversion Type     Description
    d   Signed integer decimal
    i   Signed integer decimal
    o   Signed octal value
    x   Signed hexadecimal with lowercase prefix
    X   Signed hexadecimal with uppercase prefix
    e   Floating-point exponential format with lowercase e
    E   Floating-point exponential format with uppercase E
    f   Floating-point decimal format
    F   Floating-point decimal format
    g   Floating-point format
    G   Floating-point format
    c   Single character (accepts integer or single character string)
    r   String as per calling repr()
    s   String as per calling str()
    a   String as per calling ascii()
*/

// Local Variables:
// jsonnet-library-search-directories: ("./lib")
// jsonnet-command-options:   ("--ext-str" "applist={}" "--ext-str" "dlist={ \"developer\": [{ \"email\": \"foo\"}]}")
// End:
