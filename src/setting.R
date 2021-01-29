# load
setting = yaml::yaml.load_file("./src/config.yaml")
# column name
for (name in names(setting$columns)) {
  assign(name, setting$columns[[name]])
}
# path
for (name in names(setting$path)) {
  assign(name, setting$path[[name]])
}
