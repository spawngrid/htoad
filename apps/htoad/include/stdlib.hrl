-record(init,
        {
        }).

-record(htoad,
        {
          version
        }).

-record(host,
        {
          name :: undefined | string(),
          operating_system
        }).

-record(file,
        {
          type = file :: file | dir,
          path :: undefined | string(),
          ensure :: undefined | present | absent,
          mode,
          content = ""
        }).
