handle SIGUSR1 nostop
handle SIGPIPE nostop

set pagination off
set breakpoint pending on

define bta
  thread apply all bt
end

set logging off
set logging overwrite on
set print pretty
set print elements 0

define lockon
  set scheduler-locking on
end

define lockoff
  set scheduler-locking off
end


## Frequently used breakpoints.
b __asan::ReportGenericError
b exit

set startup-with-shell off

set auto-solib-add off
set auto-load safe-path /
