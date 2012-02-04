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

-record(file,
        {
          type = file :: file | dir,
          path :: undefined | string(),
          ensure = present :: present | absent,
          mode,
          content = "",
          %% special flag denoting file request
          producer :: undefined | fs
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
