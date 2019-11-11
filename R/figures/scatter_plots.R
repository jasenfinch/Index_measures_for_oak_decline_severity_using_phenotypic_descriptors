tuneRF(DIs$data,DIs$DIs$DAI,ntreeTry = 500)

mtry <- seq(2,26,by = 4)
ntree <- c(10,100,1000,10000,100000)

a <- ntree %>%
  map(~{
    n <- .
    map(mtry,~{
      m <- .
      randomForest(DIs$data,y = DIs$DIs$DAI,ntree = n,mtry = m) %>%
        .$predicted %>%
        {abs(. - DIs$DIs$DAI)} %>%
        mean() %>%
        tibble(MAE = .)
    }) %>%
      set_names(mtry) %>%
      bind_rows(.id = 'mtry')
  }) %>%
  set_names(ntree) %>%
  bind_rows(.id = 'ntree')

DIs$data %>%
  rowid_to_column(var = 'ID') %>%
  bind_cols(select(DIs$DIs,DAI)) %>%
  select_if(is.numeric) %>%
  gather(Descriptor,Value,-ID,-DAI) %>%
  ggplot(aes(x = Value,y = DAI)) +
  geom_point(shape = 21,fill = ptol_pal()(1)) +
  theme_bw() +
  facet_wrap(~Descriptor,scales = 'free')

DIs$data %>%
  rowid_to_column(var = 'ID') %>%
  bind_cols(select(DIs$DIs,PDI)) %>%
  select_if(is.numeric) %>%
  gather(Descriptor,Value,-ID,-PDI) %>%
  ggplot(aes(x = Value,y = PDI)) +
  geom_point(shape = 21,fill = ptol_pal()(1)) +
  theme_bw() +
  facet_wrap(~Descriptor,scales = 'free')

DIs$data %>%
  select_if(is.factor) %>%
  rowid_to_column(var = 'ID') %>%
  bind_cols(select(DIs$DIs,DAI)) %>%
  gather(Descriptor,Value,-ID,-DAI) %>%
  ggplot(aes(x = Value,y = DAI)) +
  geom_point(shape = 21,fill = ptol_pal()(1)) +
  theme_bw() +
  facet_wrap(~Descriptor,scales = 'free')

DIs$data %>%
  select_if(is.factor) %>%
  rowid_to_column(var = 'ID') %>%
  bind_cols(select(DIs$DIs,PDI)) %>%
  gather(Descriptor,Value,-ID,-PDI) %>%
  ggplot(aes(x = Value,y = PDI)) +
  geom_point(shape = 21,fill = ptol_pal()(1)) +
  theme_bw() +
  facet_wrap(~Descriptor,scales = 'free')
