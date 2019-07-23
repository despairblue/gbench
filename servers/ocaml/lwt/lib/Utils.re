let rec initList = (n, item) =>
  if (n <= 0) {
    [];
  } else {
    [item, ...initList(n - 1, item)];
  };
