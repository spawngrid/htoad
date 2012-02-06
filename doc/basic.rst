Basic Concepts
==============

Hypnotoad's core consists of very few concepts; this is an intentional part of the design that allowed us to create a tool that is very easy to learn, extend and play with.

Keep in mind, though, that usage of this tool implies at least minimal knowledge of the Erlang programming language.

Fact
----

Fact is a tuple (in some cases, defined by a record) containing some logical information.

Here are few examples of facts:

.. code-block:: erlang

   {time_of_the_day, morning}
   #file{ path = "/etc/passwd"}

Facts can be asserted & retracted:

.. code-block:: erlang

   htoad:assert({time_of_the_day, morning}).
   htoad:assert([{ensure, present, #file{ path = "/tmp/hello" }},{simple_fact}]).
   htoad:retract({time_of_the_day, morning}).


Toadie
------

Toadie is a dynamically compiled Erlang module that is used to describe your deployment/provisioning scenarios. They normally bear
`.htd` extension.

The only difference from standard Erlang module is that it should not have the -module attribute and should define main/0 function that returns 
a fact or a list of facts to assert. No need export anything explicitly.

.. code-block:: erlang

   main() ->
     [
      on({host, "spawn.local"}, {role, build_server}),
      ensure(present, #file{ path = "/tmp/hello" })
     ].

Toadies get automatically imported helpers from htoad_utils module (such as on/2, ensure/2, load/1, module/1, render/2-4 and so on)

Rules
-----

Rule is a function that matches against a number of facts. If all facts match, the rule is executed. Most of the time
you won't need to write your own rules; but in cases when you will, you can add them to your toadies:

.. code-block:: erlang

   -rules([my_rule]).

   my_rule(Engine, {time_of_the_day, Time}) ->
       io:format("It is ~p!~n",[Time]),
       Engine.

It is important for rules to return the engine. Both htoad:assert/2 and htoad:retract/2 return engines as well.
