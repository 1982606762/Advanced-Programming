-module(test_emoji).

-export([test_all/0]).

% We'll use EUnit
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(testsuite(), [verbose]).

testsuite() ->
    [ {"Basic behaviour", spawn,
       [ test_start_server()
       , test_shortcode_smiley()
       , test_lookup_smiley()
       , test_lookup_nonexisting()
       , test_lookup_more()
       , test_alias_lookup()
       , test_more_aliases()
       , test_delete_shortcode()
       , test_delete_shortcode_and_alias()
       , test_delete_many_alias()
       
       ]
      }
    ].

test_start_server() ->
    {"We can call start/1 and it does not crash",
     fun () ->
       ?assertMatch({ok, _}, emoji:start([]))
     end }.

test_shortcode_smiley() ->
    {"Register new shortcode",
     fun () ->
       {ok, S} = emoji:start([]),
       ?assertEqual(ok, emoji:new_shortcode(S, "smiley",
                                            <<240,159,152,131>>))
     end }.

test_lookup_smiley() ->
    {"Register new shortcode and look it up",
    fun () ->
      {ok, S} = emoji:start([]),
      ok = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      ?assertEqual({ok,<<240,159,152,131>>}, emoji:lookup(S, "smiley"))
    end }.

test_lookup_nonexisting() ->
    {"Lookup non-existing shortcode",
    fun () ->
      {ok, S} = emoji:start([]),
      ?assertEqual(no_emoji, emoji:lookup(S, "key"))
    end }.

test_lookup_more() ->
    {"Register three shortcodes and look them up",
    fun () ->
      {ok, S} = emoji:start([]),
      ok = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      ok = emoji:new_shortcode(S, "water wave", <<240,159,140,138>>),
      ok = emoji:new_shortcode(S, "software_engineer", <<100, 100, 100, 100>>),
      ?assertEqual({ok,<<240,159,152,131>>}, emoji:lookup(S, "smiley")),
      ?assertEqual({ok,<<240,159,140,138>>}, emoji:lookup(S, "water wave")),
      ?assertEqual({ok,<<100, 100, 100, 100>>}, emoji:lookup(S, "software_engineer"))
    end }.


test_alias_lookup() ->
    {"Add an alias and look it up",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:alias(S, "smiley", "software_engineer"),
      ?assertEqual({ok,<<240,159,152,131>>}, emoji:lookup(S, "software_engineer"))
    end }.
  

test_more_aliases() ->
    {"Add two aliases and look them up",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:alias(S, "smiley", "software_engineer"),
      {ok} = emoji:alias(S, "smiley", "water wave"),
      ?assertEqual({ok,<<240,159,152,131>>}, emoji:lookup(S, "software_engineer")),
      ?assertEqual({ok,<<240,159,152,131>>}, emoji:lookup(S, "water wave"))
    end }.

test_delete_shortcode() ->
    {"Register a new shortcode and delete it",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {_} = emoji:delete(S, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
    end }.


test_delete_shortcode_and_alias() ->
    {"Register a new shortcode, then add alias and delete them",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:alias(S, "smiley", "software_engineer"),
      {_} = emoji:delete(S, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley")),
      ?assertEqual(no_emoji, emoji:lookup(S, "software_engineer"))
    end }.

test_delete_many_alias() ->
    {"Register a new shortcode, then add 2 alias, one by the other one, and delete 1st alias",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok} = emoji:new_shortcode(S, "smiley", <<240,159,152,131>>),
      {ok} = emoji:alias(S, "smiley", "software_engineer"),
      {ok} = emoji:alias(S, "software_engineer", "water wave"),
      {_} = emoji:delete(S, "software_engineer"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley")),
      ?assertEqual(no_emoji, emoji:lookup(S, "software_engineer")),
      ?assertEqual(no_emoji, emoji:lookup(S, "water wave"))
    end }.
