{application, mutex,
 [{description, "mutex"},
  {vsn, "0.1.0"},
  {modules, [mutex_app, mutex_gs, mutex_sup, client_sup, client]},
  {registered, [client_sup]},
  {applications, []},
  {mod, {mutex_app, []}}
 ]}.
