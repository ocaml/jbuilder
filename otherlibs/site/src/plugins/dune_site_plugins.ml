module Private_ = struct
  module Plugins = Plugins
  module Meta_parser = Meta_parser
end

module V1 = struct
  let load = Plugins.load

  let available = Plugins.available

  let load_script = Plugins.load_script
end
