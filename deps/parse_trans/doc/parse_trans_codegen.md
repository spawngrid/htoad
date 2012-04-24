

#Module parse_trans_codegen#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Parse transform for code generation pseduo functions.



__Authors:__ : Ulf Wiger ([`ulf.wiger@erlang-solutions.com`](mailto:ulf.wiger@erlang-solutions.com)).<a name="description"></a>

##Description##




...
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse_transform-2">parse_transform/2</a></td><td>
Searches for calls to pseudo functions in the module <code>codegen</code>,  
and converts the corresponding erlang code to a data structure  
representing the abstract form of that code.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="parse_transform-2"></a>

###parse_transform/2##




<pre>parse_transform(Forms, Options) -&gt; NewForms</pre>
<br></br>







Searches for calls to pseudo functions in the module `codegen`,  
and converts the corresponding erlang code to a data structure  
representing the abstract form of that code.



The purpose of these functions is to let the programmer write  
the actual code that is to be generated, rather than manually  
writing abstract forms, which is more error prone and cannot be  
checked by the compiler until the generated module is compiled.



Supported functions:



##gen_function/2##




Usage: `codegen:gen_function(Name, Fun)`



Substitutes the abstract code for a function with name `Name`
and the same behaviour as `Fntun`.



`Fun` can either be a anonymous `fun`, which is then converted to
a named function. It can also be an `implicit fun`, e.g.
`fun is_member/2`. In this case, the referenced function is fetched
and converted to an abstract form representation. It is also renamed
so that the generated function has the name `Name`.



##gen_functions/1##




Takes a list of `{Name, Fun}` tuples and produces a list of abstract
data objects, just as if one had written
`[codegen:gen_function(N1,F1),codegen:gen_function(N2,F2),...]`.



##exprs/1##




Usage: `codegen:exprs(Fun)`



`Fun` is either an anonymous function, or an implicit fun with only one  
function clause. This "function" takes the body of the fun and produces  
a data type representing the abstract form of the list of expressions in  
the body. The arguments of the function clause are ignored, but can be  
used to ensure that all necessary variables are known to the compiler.



##Variable substitution##




It is possible to do some limited expansion (importing a value
bound at compile-time), using the construct `{'$var', V}`, where
`V` is a bound variable in the scope of the call to `gen_function/2`.

Example:
<pre>
  gen(Name, X) ->
     codegen:gen_function(Name, fun(L) -> lists:member({'$var',X}, L) end).
  </pre>

After transformation, calling `gen(contains_17, 17)` will yield the
abstract form corresponding to:
<pre>
  contains_17(L) ->
     lists:member(17, L).
  </pre>



##Form substitution##




It is possible to inject abstract forms, using the construct
`{'$form', F}`, where `F` is bound to a parsed form in
the scope of the call to `gen_function/2`.

Example:
<pre>
  gen(Name, F) ->
     codegen:gen_function(Name, fun(X) -> X =:= {'$form',F} end).
  </pre>

After transformation, calling `gen(is_foo, {atom,0,foo})` will yield the
abstract form corresponding to:
<pre>
  is_foo(X) ->
     X =:= foo.
  </pre>