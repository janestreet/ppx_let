let package_name = "ppx_let"

let sections =
  [ ("lib",
    [ ("built_lib_ppx_let", None)
    ],
    [ ("META", None)
    ])
  ; ("bin",
    [ ("built_exec_ppx", Some "../lib/ppx_let/ppx")
    ],
    [])
  ]
