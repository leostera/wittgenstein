-module(meta_tools).

-export([
         all_applications/0,
         all_module_attributes/0,
         all_modules/0,
         modules_implementing_behavior/1
        ]).

all_applications() ->
  lists:map(fun ({Name, _, _}) -> Name end, application:loaded_applications()).

all_modules() ->
  Mods = lists:map(fun (M) -> {ok, Mods} = application:get_key(M, modules), Mods end,
                  all_applications()),
  lists:flatten(Mods).

all_module_attributes() ->
  lists:map(fun (M) -> #{ module => M:module_info(module),
                          attrs  => maps:from_list(M:module_info(attributes))
                        }
            end, all_modules()).

-spec modules_implementing_behavior(atom()) -> [atom()].
modules_implementing_behavior(BehaviorName) ->
  lists:filtermap(fun (#{ module := M, attrs := Attrs }) ->
                      Bs = maps:get(behavior, Attrs, []),
                      case lists:member(BehaviorName, Bs) of
                        false -> false;
                        true  -> {true, M}
                      end
                  end, all_module_attributes()).
