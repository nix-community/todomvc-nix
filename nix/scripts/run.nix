{ writeScript, bash, backend, frontend, caddy, xdg_utils }:
writeScript "todomvc-run" ''
#!${bash}/bin/bash
#
# Build and run the services locally
#
set -euo pipefail

export PORT=3000
export URL=http://localhost:$PORT
frontend_url="http://localhost:2015/?$URL/todos"

echo Starting backend on port $PORT
${backend}/bin/todobackend-scotty &

echo Starting frontend on port 2015
cd ${frontend}/var/www
${caddy}/bin/caddy &

${xdg_utils}/bin/xdg-open "$frontend_url"

wait
''
