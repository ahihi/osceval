# osceval

evaluate Emacs Lisp code received over OSC. useful for controlling Emacs externally, e.g. using hardware controllers. (i use it to [manipulate](https://youtu.be/cmgvn2MuQ-g?si=XbPsxf3Yb0sftUO5&t=290) [TidalCycles](https://tidalcycles.org/) patterns)

> [!CAUTION]
> careless configuration of osceval is a security risk — it will open up your system to remote code execution attacks!

## first, consider `emacsclient`

Emacs' built-in [`emacsclient`](https://www.emacswiki.org/emacs/EmacsClient) can also be used to control Emacs externally:

```shell
emacsclient --eval '(message "hello")'
```

using osceval might be preferable when any of the following apply:

- the controlling program runs on a different machine than Emacs
- the latency overhead of starting the `emacsclient` process and connecting to the Emacs server is too large

## installation

you will also need the [osc](https://elpa.gnu.org/packages/osc.html) package.

using [straight.el](https://github.com/radian-software/straight.el):

```elisp
(use-package osceval
  :straight (osceval :type git :host github :repo "ahihi/osceval"))
```

or manually: place `osceval.el` in your `load-path`, then run

```elisp
(require 'osceval)
```

## usage

start osceval: `M-x osceval-start`

stop osceval: `M-x osceval-stop`

## configuration

osceval can be configured via the following configuration variables:

### `osceval-host`

IP address on which to listen for OSC messages (default: `127.0.0.1`).

### `osceval-port`

Port on which to listen for OSC messages (default: `9473`).

### `osceval-buffer`

The name of the osceval process buffer (default: `*osceval*`).

### `osceval-handler`

Handler function to run on incoming OSC messages (default: `#'osceval-default-handler`).

the default handler handles messages of the form `/eval <code>`, directly evaluating the given code.

in many cases, it is useful to evaluate code in an interactive context, in the currently active window's buffer. the following custom handler defines an additional `/eval-in-window` message which achieves this:

```elisp
(defun pulu-osceval-handler (path &rest args)
  (pcase path
    ("/eval" (let ((code (nth 0 args)))
               (osceval-message "%s %s" path code)
               (eval (read code))))
    ("/eval-in-window" (let ((code (nth 0 args)))
                         (osceval-message "%s %s" path code)
                         (eval `(call-interactively
                                 (lambda ()
                                   (interactive)
                                   (with-current-buffer (window-buffer)
                                     ,(read code)
                                     (run-hooks 'post-command-hook)))))))
    (t nil)))
(setq osceval-handler #'pulu-osceval-handler)
```

(note: the function `osceval-message` is used to print messages in the osceval process buffer)
