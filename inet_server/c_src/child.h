
void child_main(Server *serv, int listen_fd, int extra);
void child_establish_backend_connection(Server *me, int fd);
void shutdown_child_client(Server *me, int fd);
void delete_client(Server *me, int fd);
