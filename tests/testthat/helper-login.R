
cred <- load_config(section = "openbis")
tok <- login_openbis(cred$username, cred$password)