library(keyring)

# Set a password for the localdb service
key_set(service = "localdb", 
                   username = "root")

key_get("localdb", "root")
