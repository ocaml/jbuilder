value rec list_length = fun
  [ [] ->  0
  | [ _ :: tl] -> 1 + list_length tl
  ];
