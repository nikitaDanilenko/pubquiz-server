# General idea

1. The front end addresses the main page without a given port, 
   hence pretending the server is running directly on the page itself, with the usual SSL port 443.

   For this to work, the front end in the production environment does not use any ports at all.
   This means that the file `src/Common/Constants.elm` is adjusted in production such that the port is unused.
1. The actual service is running locally only, which means that it is inaccessible from outside.
1. The reverse proxy redirects anything from `/api` to `http://127.0.0.1:8000/api`
   Note the missing SSL layer, which is now redundant, because the actual communication with the service
   now only happens on the server locally.

   For the redirection to work, we have the following lines in the file `/etc/apache2/sites-available/000-default.conf`
   under the port 443:

   ```
   # Reverse Proxy Setup
   ProxyPreserveHost On

   ProxyPass /api http://127.0.0.1:8000/api
   ProxyPassReverse /api http://127.0.0.1:8000/api
   ```

   For future reference, it may be a good idea to use a common prefix for *all* possible routes,
   while at least one of the routes is currently located directly under `api`.