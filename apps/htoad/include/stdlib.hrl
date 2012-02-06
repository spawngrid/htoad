-record(init,
        {
        }).

-record('htoad.toadie',
        {
          module,
          filename,
          server
        }).

-record(htoad,
        {
          version
        }).

%%
-record(user, 
        {
          id,
          name
        }).

-record(group, 
        {
          id,
          name
        }).

-record(file,
        {
          type = file :: file | dir,
          path :: undefined | string(),
          user :: #user{} | string() | integer(),
          group :: #group{} | string() | integer(),
          mode,
          content = ""
        }).

-record(package,
        {
          name,
          version
        }).

-record(shell,
        {
          cmd,
          run_as :: undefined | superuser | term()
        }).

-record(error_report,
        {
          rule :: {module(), atom()},
          fact :: tuple(),
          reason :: term()
        }).
