def build_server do
  file = {:file, dir, Erlang.filename.join('/tmp/srv','build_dir'), :undefined, :undefined, :undefined, ""},
  [
   ensure(:present, file),
   on([ {:operating_system_name, :darwin},
        {:package_manager, :brew} ],
    ensure(:present, {:package, 'git', :undefined}))
  ]
end

def main do
  [
   load('base.htd')
   on({:host, 'spawn.local'}, {:role, :build_server}),
   on({:role, :build_server}, build_server)
  ]
end
