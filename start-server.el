(require 'server)
(setq server-use-tcp t
      server-host "<IP>"
      server-socket-dir "~/.emacs.d/server")
(unless (server-running-p)
    (server-start))
