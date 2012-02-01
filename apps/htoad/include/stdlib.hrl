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

-record(host,
        {
          name :: undefined | string()
        }).

-record(file,
        {
          type = file :: file | dir,
          path :: undefined | string(),
          ensure = present :: present | absent,
          mode,
          content = ""
        }).
