# Reviewed lifecycle rollout — approval required

Status: source integrated and isolated tests run; **not deployed or configured**.
No production control directory, account, secret, environment setting, service,
Tunnel route, Cloudflare deployment or permission change has been made.

## Architecture and inspected baseline

- CT 104: analysis source at
  `/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data`,
  base `f35ab4f6538141402b1349e2cee09fc1c245ccf1`; ingestion sibling
  `Sun_Data_Analytics_Talent_Repo`, base `124814ab0d48f17caf0f94305fb49986b00f4dc4`.
  All 66 handoff source files matched the verified archive before integration.
  Existing unrelated ingestion wrappers and generated catalog edits were preserved.
- CT 105: permission app `/srv/projects/sun_Data_analytics_apps`, branch `main`,
  base `e3554a85b4a380c9c8b595df71fa412c87d56371`. Analysis checkout base
  `4fc79cd4ba742032f1f22e5bf383e4d79c0d8188` and its unrelated edits are untouched.
- Both unprivileged containers map UID/GID 0..65535 to host 100000..165535.
  Both already have `jonathon` UID/GID 1000. The same ext4 disk is mounted at
  `/mnt/storage/local/hdd01`. No new Proxmox mount/configuration is needed.
- Authority: one JSON file per code at
  `/mnt/storage/local/hdd01/DataLake/Sun_Data_Analytics/Talent_data/Controls/talents`.
  This directory was absent. No lifecycle root was found in either backend repo
  environment, the nightly service, the three installed processing user services,
  or their referenced environment files; all four effective service checks succeeded.
  The existing subtitle-backfill `Logs/.../control` directory is unrelated and retained.
- The bridge binds **100.118.151.32:8765** on CT 104. CT 105's existing remotely
  managed Tunnel gains only `talent-control-origin.sun-dataanalytics.com` forwarding
  to that address over Tailscale. Its current routes are preserved.
- Cloudflare's existing Worker verifies Access and administrator identity before
  proxying lifecycle requests. The same permission admin page provides the controls.
  D1 stores no lifecycle state. The client talent dashboard receives no changes.

## Inputs to resolve before approval/execution

Obtain the existing CT 105 Tunnel's `CF_ACCOUNT_ID`, `CF_TUNNEL_ID`, `CF_ZONE_ID`
and a suitably scoped Cloudflare API token through the existing administration
channel. These identifiers and live Cloudflare topology were not fetched during
source testing. Confirm that the new origin hostname is not covered by an
interactive Access login policy; its authentication is the separate bridge secret.
If a wildcard Access policy covers it, stop and review that topology rather than
weakening any existing policy. Confirm CT 105 can reach CT 104 over the tailnet.

Record the current Worker version with `wrangler deployments list` and the
current published `gh-pages` commit. They are required rollback inputs. Never
print `.env`, bearer tokens, Tunnel tokens or credential files. Do not enable
shell tracing for the following commands.

## 1. Recheck and drain maintained workers — CT 104, root

Recheck Git status/revisions and the guarded source manifests before rollout.
Do not apply the handoff patches again: they were already present on CT 104.
Do not reset or overwrite either working tree.

```bash
set -eu
ANALYSIS=/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Analyze_Talent_Data
INGEST=/home/jonathon/sun_data_analytics_projects/Sun_Data_Analytics_Talent_Repo
LAKE=/mnt/storage/local/hdd01/DataLake/Sun_Data_Analytics/Talent_data
CONTROL="$LAKE/Controls/talents"
BACKUP=/root/talent-lifecycle-rollout-20261027
mountpoint -q /mnt/storage/local/hdd01
test ! -e "$CONTROL"
# Existing authority, if discovered, requires a reviewed preservation/migration plan.
python3 - <<'PY'
from pathlib import Path
for name in ('Sun_Data_Analytics_Analyze_Talent_Data', 'Sun_Data_Analytics_Talent_Repo'):
    p = Path('/home/jonathon/sun_data_analytics_projects') / name / '.env'
    for line in p.read_text().splitlines():
        key, sep, value = line.removeprefix('export ').partition('=')
        if sep and key.strip() == 'TALENT_CONTROL_ROOT' and value.strip().strip('"\''):
            raise SystemExit('Existing lifecycle configuration found; stop and preserve authority')
PY
if systemctl is-active --quiet talent_analytics_nightly.service; then
  echo 'Nightly work is active; wait for normal completion before proceeding' >&2
  exit 75
fi
for unit in sun-data-subtitle-backfill.service sun-data-title-classification-check.service sun-data-title-classification-weekly.service; do
  if runuser -u jonathon -- env XDG_RUNTIME_DIR=/run/user/1000 systemctl --user is-active --quiet "$unit"; then
    echo "$unit is active; wait for normal completion" >&2
    exit 75
  fi
done
```

Before directory creation, inspect effective `Environment` and `EnvironmentFiles`
for the nightly service and the three installed user services above, filtering for
`TALENT_CONTROL_ROOT` without printing other variables. Follow referenced environment
files with the same filter. Stop if another configured root is found, and locate
and preserve that authority. Failure to query an installed service is a blocker;
do not infer absence from a failed query.

Also confirm manually launched maintained jobs have drained. These commands do
not certify custom notebooks or submitted remote batches. Do not kill jobs or
change real talent switches to make this gate pass. Existing long-lived processes
without the new environment must finish and be relaunched with it before writes
are enabled. No unattended restart of active workers is part of this plan.

## 2. Narrow filesystem access and worker enforcement — CT 104, root

The new account owns only control writes. Existing GID 1000 is the read group;
no CT 105 account/group/permission change is needed. The writer is not added to
the `jonathon` group. Setgid inheritance gives records GID 1000, mode 0640.
The inspected catalog file is already world-readable (0664); it stays read-only
for the bridge. Do not broaden catalog, home, raw-data or credential permissions.

```bash
# Continue in the same shell with ANALYSIS, INGEST, LAKE, CONTROL, BACKUP defined.
test ! -e "$BACKUP"
install -d -m 0700 "$BACKUP"
cp -p "$ANALYSIS/.env" "$BACKUP/analysis.env"
cp -p "$INGEST/.env" "$BACKUP/ingestion.env"
if getent passwd sun-data-lifecycle >/dev/null || getent passwd 60001 >/dev/null || getent group 60001 >/dev/null; then
  echo 'Dedicated account/ID already exists; review before proceeding' >&2
  exit 75
fi
groupadd --gid 60001 sun-data-lifecycle
useradd --system --uid 60001 --gid sun-data-lifecycle --no-create-home --shell /usr/sbin/nologin sun-data-lifecycle
# Refuse existing paths; never overwrite or reinitialize controls.
test ! -e "$LAKE/Controls"
install -d -o root -g root -m 0755 "$LAKE/Controls"
install -d -o sun-data-lifecycle -g jonathon -m 2750 "$CONTROL"
python3 - <<'PY'
from pathlib import Path
base = Path('/home/jonathon/sun_data_analytics_projects')
addition = '\nTALENT_CONTROL_ROOT=/mnt/storage/local/hdd01/DataLake/Sun_Data_Analytics/Talent_data/Controls/talents\nTALENT_CONTROL_MOUNT=/mnt/storage/local/hdd01\n'
for repo in ('Sun_Data_Analytics_Analyze_Talent_Data', 'Sun_Data_Analytics_Talent_Repo'):
    path = base / repo / '.env'
    with path.open('a') as output:
        output.write(addition)
PY
# New maintained worker processes read these settings through existing env loaders.
# Do not enable the old local Shiny writer or restart active worker processes.
```

Verify newly launched maintained entrypoints load the root and mount (print paths
only, never all environment values). Run the focused tests below with temporary
roots overriding production configuration. Workers are now the first rollout
component; writes remain disabled throughout origin and UI deployment.

## 3. Stage the authenticated bridge — CT 104, root

```bash
install -d -o root -g root -m 0755 /etc/sun-data
# A newly created root-only file: secret never goes to argv, stdout or Git.
python3 - <<'PY'
import os, secrets
from pathlib import Path
path = Path('/etc/sun-data/talent-control-bridge.env')
fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
with os.fdopen(fd, 'w') as output:
    output.write('TALENT_CONTROL_ROOT=/mnt/storage/local/hdd01/DataLake/Sun_Data_Analytics/Talent_data/Controls/talents\n')
    output.write('TALENT_CONTROL_MOUNT=/mnt/storage/local/hdd01\n')
    output.write('UNIFIED_CATALOG_DB_PATH=/mnt/storage/local/hdd01/DataLake/Sun_Data_Analytics/Talent_data/Data_lakehouse/talent_lakehouse.duckdb\n')
    output.write('TALENT_BRIDGE_HOST=100.118.151.32\nTALENT_BRIDGE_PORT=8765\n')
    output.write('TALENT_BRIDGE_SECRET=' + secrets.token_hex(48) + '\n')
PY
test ! -e /etc/systemd/system/sun-data-talent-control-bridge.service
install -o root -g root -m 0644 "$ANALYSIS/config/systemd/sun-data-talent-control-bridge.service" /etc/systemd/system/
systemd-analyze verify /etc/systemd/system/sun-data-talent-control-bridge.service
systemctl daemon-reload
systemctl enable --now sun-data-talent-control-bridge.service
# Read-only smoke check, authenticated and unauthenticated; prints counts only.
python3 - <<'PY'
import urllib.request, urllib.error, json
from pathlib import Path
env = dict(line.split('=', 1) for line in Path('/etc/sun-data/talent-control-bridge.env').read_text().splitlines())
url = 'http://100.118.151.32:8765/v1/talents'
try:
    urllib.request.urlopen(url, timeout=10)
    raise SystemExit('Unauthenticated origin unexpectedly allowed access')
except urllib.error.HTTPError as error:
    assert error.code == 401
request = urllib.request.Request(url, headers={'Authorization': 'Bearer ' + env['TALENT_BRIDGE_SECRET'], 'X-SDA-Lifecycle-Actor': 'rollout-read-only@example.invalid'})
with urllib.request.urlopen(request, timeout=10) as response:
    print('Authenticated status:', response.status, 'talent count:', len(json.load(response)['talents']))
PY
```

The service sandbox is staged source, not yet tested under production systemd
isolation. If namespace restrictions in this unprivileged LXC prevent startup,
stop and review the specific restriction; do not disable the sandbox wholesale.
If the canonical DuckDB has an incompatible writer lock, the bridge returns 503;
retry after that writer finishes. It does not open a writable catalog connection.

## 4. Add one route to the existing remotely managed Tunnel

Run the following on the approval executor with the existing Tunnel identifiers
exported. Supply the scoped token through a protected environment, not command
arguments or logs. The script refuses an existing DNS record/hostname, saves the
old config and the new DNS ID in a private ledger, and preserves other routes.
Use a fresh shell with tracing disabled. Do not run this until steps 1–3 pass.

```bash
export CF_ACCOUNT_ID='REVIEW_EXISTING_ACCOUNT_ID'
export CF_TUNNEL_ID='REVIEW_EXISTING_CT105_TUNNEL_ID'
export CF_ZONE_ID='REVIEW_SUN_DATAANALYTICS_ZONE_ID'
# CLOUDFLARE_API_TOKEN must already be set through the protected admin channel.
python3 - <<'PY'
import json, os, urllib.request
from pathlib import Path
host = 'talent-control-origin.sun-dataanalytics.com'
base = 'https://api.cloudflare.com/client/v4'
a, t, z = (os.environ[k] for k in ('CF_ACCOUNT_ID', 'CF_TUNNEL_ID', 'CF_ZONE_ID'))
def api(path, method='GET', body=None):
    req = urllib.request.Request(base + path, method=method,
        headers={'Authorization': 'Bearer ' + os.environ['CLOUDFLARE_API_TOKEN'], 'Content-Type': 'application/json'},
        data=json.dumps(body).encode() if body is not None else None)
    with urllib.request.urlopen(req, timeout=30) as response:
        result = json.load(response)
    if not result.get('success'): raise RuntimeError('Cloudflare request failed; inspect privately')
    return result['result']
path = f'/accounts/{a}/cfd_tunnel/{t}/configurations'
old = api(path)['config']
assert not any(rule.get('hostname') == host for rule in old['ingress'])
assert not api(f'/zones/{z}/dns_records?name={host}')
ledger = Path('/root/talent-lifecycle-cloudflare-route.json')
fd = os.open(ledger, os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o600)
with os.fdopen(fd, 'w') as output: json.dump({'previous_config': old, 'tunnel': t, 'zone': z}, output)
record = api(f'/zones/{z}/dns_records', 'POST', {'type': 'CNAME', 'name': host, 'content': t + '.cfargotunnel.com', 'proxied': True, 'ttl': 1})
state = json.loads(ledger.read_text()); state['dns_id'] = record['id']; ledger.write_text(json.dumps(state))
config = {**old, 'ingress': [{'hostname': host, 'service': 'http://100.118.151.32:8765'}, *old['ingress']]}
api(path, 'PUT', {'config': config})
print('Added only the reviewed lifecycle origin route and DNS record')
PY
```

The [Tunnel configuration API](https://developers.cloudflare.com/api/resources/zero_trust/subresources/tunnels/subresources/cloudflared/)
and [DNS API](https://developers.cloudflare.com/api/resources/dns/subresources/records/methods/create/)
are the official configuration interfaces. No Tunnel restart or Proxmox change
is needed for the remote configuration update. Verify unauthorized HTTPS origin
requests return 401 and an authenticated GET through the Worker succeeds before
enabling writes. Never POST a real talent as a deployment smoke test.

## 5. Deploy Worker with writes disabled, publish admin frontend, enable last

As `jonathon` in CT 105:

```bash
export PATH=/home/jonathon/.nvm/versions/node/v24.18.0/bin:$PATH
cd /srv/projects/sun_Data_analytics_apps
node_modules/.bin/wrangler deployments list
# Record PREVIOUS_WORKER_VERSION and the published gh-pages commit externally.
npm run test:worker
node --check site/assets/js/admin-lifecycle.js
git diff --check
```

Transfer the bridge secret directly from CT 104's protected file to Wrangler stdin
on CT 105 using the approval executor (root on the Proxmox host). It must never
appear in stdout, argv or a repository file. This is a production secret operation:

```bash
pct exec 104 -- python3 -c 'from pathlib import Path; e=dict(l.split("=",1) for l in Path("/etc/sun-data/talent-control-bridge.env").read_text().splitlines()); print(e["TALENT_BRIDGE_SECRET"])' |
  pct exec 105 -- runuser -u jonathon -- /home/jonathon/.nvm/versions/node/v24.18.0/bin/node /srv/projects/sun_Data_analytics_apps/node_modules/wrangler/bin/wrangler.js secret put TALENT_BRIDGE_SECRET --cwd /srv/projects/sun_Data_analytics_apps
```

The pipe delivers secret bytes directly to Wrangler; do not run its left-hand
command alone or capture/tee pipeline output. The secret upload may deploy a
Worker version; capture the pre-upload rollback version first. Then, as `jonathon`
in CT 105:

```bash
node_modules/.bin/wrangler deploy --keep-vars \
  --var TALENT_LIFECYCLE_ORIGIN:https://talent-control-origin.sun-dataanalytics.com \
  --var TALENT_LIFECYCLE_WRITES_ENABLED:false
```

Use the Access-authenticated admin page/API to perform a read-only status check.
Confirm that the direct origin rejects unauthenticated requests and that controls
are disabled. Confirm newly launched backend workers use the configured shared
root and that missing-mount/directory fixture checks fail closed. Do not test by
changing any real talent or client state.

Publish only the reviewed app source paths. The existing `main` push runs the
GitHub Actions Quarto publish workflow. Do not stage unrelated edits:

```bash
git add README.md site/admin.qmd site/assets/js/admin-lifecycle.js site/css/styles/admin.css \
  worker/src/index.js worker/src/lifecycle.js worker/test/lifecycle.test.js worker/test/lifecycle-ui.test.js
git diff --cached --check
git commit -m 'Integrate administrator talent lifecycle controls with origin writer'
LIFECYCLE_APP_COMMIT=$(git rev-parse HEAD)
# Save this commit ID with the deployment record for the forward revert.
git push origin HEAD:main
```

Wait for the existing site publication workflow to succeed and verify the admin
page. Only after all preceding gates pass, enable writes with the already tested
Worker source:

```bash
node_modules/.bin/wrangler deploy --keep-vars \
  --var TALENT_LIFECYCLE_ORIGIN:https://talent-control-origin.sun-dataanalytics.com \
  --var TALENT_LIFECYCLE_WRITES_ENABLED:true
```

This final command enables the capability; it must not be followed by an
unsolicited real talent switch or client permission edit.

## Rollback — retain authority and enforcement

1. In CT 105 as `jonathon`, immediately disable new UI writes with the tested
   Worker source (or roll back directly to the recorded previous Worker version):

```bash
export PATH=/home/jonathon/.nvm/versions/node/v24.18.0/bin:$PATH
cd /srv/projects/sun_Data_analytics_apps
node_modules/.bin/wrangler deploy --keep-vars \
  --var TALENT_LIFECYCLE_ORIGIN:https://talent-control-origin.sun-dataanalytics.com \
  --var TALENT_LIFECYCLE_WRITES_ENABLED:false
# PREVIOUS_WORKER_VERSION is the exact version captured before secret upload.
node_modules/.bin/wrangler rollback "$PREVIOUS_WORKER_VERSION" --yes \
  --message 'Roll back lifecycle frontend and origin proxy'
```

2. On CT 104 as root, stop only the newly added bridge:

```bash
systemctl disable --now sun-data-talent-control-bridge.service
```

3. If the admin frontend must also be reverted, create a forward Git revert of
   only the lifecycle integration commit, then publish it through the existing
   workflow. Do not force-push or reset branches:

```bash
# CT 105, as jonathon; require a clean working tree before this operation.
git revert "$LIFECYCLE_APP_COMMIT"
git push origin HEAD:main
```

4. Remove only this rollout's origin route and DNS record, using the private
   ledger and the same protected API environment. Refresh the current config
   first so unrelated subsequent changes are preserved:

```bash
python3 - <<'PY'
import json, os, urllib.request
from pathlib import Path
state = json.loads(Path('/root/talent-lifecycle-cloudflare-route.json').read_text())
host = 'talent-control-origin.sun-dataanalytics.com'
a, t, z = os.environ['CF_ACCOUNT_ID'], state['tunnel'], state['zone']
def api(path, method='GET', body=None):
    request = urllib.request.Request('https://api.cloudflare.com/client/v4' + path, method=method,
        headers={'Authorization': 'Bearer ' + os.environ['CLOUDFLARE_API_TOKEN'], 'Content-Type': 'application/json'},
        data=json.dumps(body).encode() if body is not None else None)
    with urllib.request.urlopen(request, timeout=30) as response: result = json.load(response)
    if not result.get('success'): raise RuntimeError('Cloudflare rollback failed; inspect privately')
    return result['result']
path = f'/accounts/{a}/cfd_tunnel/{t}/configurations'
config = api(path)['config']
ours = [rule for rule in config['ingress'] if rule.get('hostname') == host]
assert len(ours) <= 1
if ours:
    assert ours[0] == {'hostname': host, 'service': 'http://100.118.151.32:8765'}, 'Route changed; stop and review'
    api(path, 'PUT', {'config': {**config, 'ingress': [rule for rule in config['ingress'] if rule not in ours]}})
if state.get('dns_id'):
    recordpath = f'/zones/{z}/dns_records/{state["dns_id"]}'
    record = api(recordpath)
    assert record['name'] == host and record['content'] == t + '.cfargotunnel.com'
    api(recordpath, 'DELETE')
print('Removed only this rollout origin route and its DNS record')
PY
```

Leave worker enforcement, control JSON, the dedicated account and restrictive
filesystem modes in place. Do not restore old JSON revisions, delete records,
unset `TALENT_CONTROL_ROOT`, or revert guarded worker code as a routine rollback:
that can re-enable archived/paused work. If enforcement itself fails, keep affected
workers quiescent and fix forward; review any temporary launch changes separately.
Secrets and backup environment files remain protected until explicitly retired.

## Isolated verification and limits

On CT 104 from the analysis repository:

```bash
RENV_CONFIG_AUTOLOADER_ENABLED=FALSE .venv/bin/python -m unittest \
  py_scripts.tests.test_talent_lifecycle py_scripts.tests.test_talent_control_bridge
RENV_CONFIG_AUTOLOADER_ENABLED=FALSE Rscript --vanilla r_scripts/tests/test_viewer_activity_publish.R
```

From the ingestion repository: `PYTHONPATH=. .venv/bin/python -m unittest tests.test_lifecycle`.
From the permission app: `npm run test:worker` and the JS syntax check above.
These tests use temporary control roots and disposable DuckDB fixtures.

The cross-container test used only the isolated shared directory
`/mnt/storage/local/hdd01/stash_archives/talent-lifecycle-fixture-a290qrnt`.
A lock held by CT 104 blocked CT 105, CT 105 observed all 41 atomically saved
revisions without partial JSON, and both containers read the identical final
record. This proves shared-filesystem behavior under the test identities; runtime
account permissions and production sandbox startup still require rollout checks.
No synthetic record was placed in the canonical production control directory.

Archive only blocks retrieval. Pause only blocks processing. Restore/Resume are
independent; existing exclusions remain effective. Results, raw history, OAuth,
checkpoints, catalog visibility and client grants are preserved. An in-flight HTTP
request/download, admitted prompt job or remote batch may finish; controls are
cooperative checks at maintained boundaries. Custom notebooks and old processes
without configured policy are outside that coverage. A saved pause is never proof
that every running job has stopped. Removal is outside this integration.
