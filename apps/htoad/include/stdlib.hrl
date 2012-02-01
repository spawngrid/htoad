-record(init,
        {
        }).

-record('htoad.module',
        {
          module,
          filename
        }).

-record(htoad,
        {
          version
        }).

%%

-record(file,
        {
          type = file :: file | dir,
          path :: undefined | string(),
          ensure = present :: present | absent,
          mode,
          content = ""
        }).

-record(package,
        {
          name,
          version,
          ensure = present :: present | absent
        }).

-record(shell,
        {
          cmd
        }).
