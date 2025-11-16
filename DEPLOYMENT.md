# HappyShopr Deployment Guide

Complete guide for deploying HappyShopr to a production VM.

## Prerequisites

### Server Requirements

- **OS:** Ubuntu 20.04+ / Debian 10+ / CentOS 8+ / RHEL 8+
- **RAM:** 512MB minimum, 1GB+ recommended
- **Disk:** 1GB free space
- **Network:** Open port 8080 (or your chosen port)

### Install Erlang/OTP and rebar3

#### Ubuntu/Debian

```bash
# Add Erlang Solutions repository
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb
sudo apt-get update

# Install Erlang
sudo apt-get install erlang

# Install rebar3
wget https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
sudo mv rebar3 /usr/local/bin/
```

#### CentOS/RHEL

```bash
# Add EPEL repository
sudo yum install epel-release

# Install Erlang
sudo yum install erlang

# Install rebar3
wget https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
sudo mv rebar3 /usr/local/bin/
```

#### macOS (Development)

```bash
brew install erlang rebar3
```

## Deployment Steps

### 1. Build the Release

On your **development machine** or **CI/CD pipeline**:

```bash
# Clone the repository
git clone <your-repo-url>
cd HappyShopr

# Build production release
rebar3 as prod release

# The release will be in _build/prod/rel/happyshopr/
```

### 2. Transfer to Server

```bash
# Create tarball
cd _build/prod/rel
tar -czf happyshopr-1.0.0.tar.gz happyshopr/

# Copy to server
scp happyshopr-1.0.0.tar.gz user@your-server:/tmp/
```

### 3. Server Setup

SSH into your server and run:

```bash
# Create application user
sudo useradd -r -m -s /bin/false happyshopr

# Create directories
sudo mkdir -p /opt/happyshopr
sudo mkdir -p /var/lib/happyshopr/mnesia
sudo mkdir -p /var/log/happyshopr

# Extract release
cd /opt/happyshopr
sudo tar -xzf /tmp/happyshopr-1.0.0.tar.gz --strip-components=1

# Set ownership
sudo chown -R happyshopr:happyshopr /opt/happyshopr
sudo chown -R happyshopr:happyshopr /var/lib/happyshopr
sudo chown -R happyshopr:happyshopr /var/log/happyshopr

# Set permissions
sudo chmod -R 755 /opt/happyshopr
```

### 4. Configure the Application

#### Update sys.config

```bash
sudo -u happyshopr nano /opt/happyshopr/releases/1.0.0/sys.config
```

Change these settings:

```erlang
[
    {happyshopr, [
        {http_port, 8080},  % Or your preferred port
        {api_keys, [
            {<<"YOUR-SECURE-API-KEY-HERE">>, <<"default">>}
        ]},
        {mnesia_dir, "/var/lib/happyshopr/mnesia"}
    ]},
    {mnesia, [
        {dir, "/var/lib/happyshopr/mnesia"}
    ]}
].
```

**Generate a secure API key:**
```bash
openssl rand -base64 32
```

#### Update vm.args

```bash
sudo -u happyshopr nano /opt/happyshopr/releases/1.0.0/vm.args
```

Change the node name to match your server:

```
-name happyshopr@your-server.com
-setcookie YOUR_SECURE_COOKIE_HERE
```

**Generate a secure cookie:**
```bash
openssl rand -base64 24
```

### 5. Create systemd Service

Create `/etc/systemd/system/happyshopr.service`:

```bash
sudo nano /etc/systemd/system/happyshopr.service
```

Add this content:

```ini
[Unit]
Description=HappyShopr Shopping List Service
Documentation=https://github.com/yourusername/HappyShopr
After=network.target

[Service]
Type=forking
User=happyshopr
Group=happyshopr
WorkingDirectory=/opt/happyshopr
Environment="HOME=/var/lib/happyshopr"

# Start the application
ExecStart=/opt/happyshopr/bin/happyshopr start

# Stop the application
ExecStop=/opt/happyshopr/bin/happyshopr stop

# Restart policy
Restart=on-failure
RestartSec=5
KillMode=process

# Security settings
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ProtectHome=true
ReadWritePaths=/var/lib/happyshopr /var/log/happyshopr

# Limits
LimitNOFILE=65536

[Install]
WantedBy=multi-user.target
```

### 6. Enable and Start Service

```bash
# Reload systemd
sudo systemctl daemon-reload

# Enable service to start on boot
sudo systemctl enable happyshopr

# Start the service
sudo systemctl start happyshopr

# Check status
sudo systemctl status happyshopr
```

### 7. Verify Deployment

```bash
# Check if service is running
sudo systemctl status happyshopr

# Test health endpoint
curl http://localhost:8080/health

# View logs
sudo journalctl -u happyshopr -f

# Check Erlang process
ps aux | grep beam
```

Expected health response:
```json
{
  "status": "ok",
  "version": "1.0.0",
  "uptime_seconds": 123
}
```

## Firewall Configuration

### UFW (Ubuntu/Debian)

```bash
sudo ufw allow 8080/tcp
sudo ufw reload
```

### firewalld (CentOS/RHEL)

```bash
sudo firewall-cmd --permanent --add-port=8080/tcp
sudo firewall-cmd --reload
```

### iptables

```bash
sudo iptables -A INPUT -p tcp --dport 8080 -j ACCEPT
sudo iptables-save | sudo tee /etc/iptables/rules.v4
```

## Nginx Reverse Proxy (Optional)

If you want to use HTTPS or a custom domain:

### Install Nginx

```bash
sudo apt-get install nginx certbot python3-certbot-nginx
```

### Configure Nginx

Create `/etc/nginx/sites-available/happyshopr`:

```nginx
server {
    listen 80;
    server_name your-domain.com;

    location / {
        proxy_pass http://localhost:8080;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_cache_bypass $http_upgrade;
    }
}
```

Enable the site:

```bash
sudo ln -s /etc/nginx/sites-available/happyshopr /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl reload nginx
```

### Add SSL with Let's Encrypt

```bash
sudo certbot --nginx -d your-domain.com
```

## Monitoring and Maintenance

### View Logs

```bash
# Systemd logs
sudo journalctl -u happyshopr -f

# Last 100 lines
sudo journalctl -u happyshopr -n 100

# Since yesterday
sudo journalctl -u happyshopr --since yesterday

# Application console (connects to running node)
sudo -u happyshopr /opt/happyshopr/bin/happyshopr remote_console
# Press Ctrl+D to exit without stopping the application
```

### Service Management

```bash
# Start
sudo systemctl start happyshopr

# Stop
sudo systemctl stop happyshopr

# Restart
sudo systemctl restart happyshopr

# Status
sudo systemctl status happyshopr

# Enable auto-start on boot
sudo systemctl enable happyshopr

# Disable auto-start
sudo systemctl disable happyshopr
```

### Backup Mnesia Database

```bash
# Create backup directory
sudo mkdir -p /var/backups/happyshopr

# Backup script
sudo -u happyshopr /opt/happyshopr/bin/happyshopr eval \
  'mnesia:backup("/var/backups/happyshopr/mnesia-backup-$(date +%Y%m%d-%H%M%S)").'

# Or simply copy the data directory
sudo cp -r /var/lib/happyshopr/mnesia /var/backups/happyshopr/mnesia-$(date +%Y%m%d-%H%M%S)
```

### Restore Mnesia Database

```bash
# Stop the application
sudo systemctl stop happyshopr

# Restore from backup
sudo cp -r /var/backups/happyshopr/mnesia-20231110-120000/* /var/lib/happyshopr/mnesia/

# Or use Mnesia restore
sudo -u happyshopr /opt/happyshopr/bin/happyshopr eval \
  'mnesia:restore("/var/backups/happyshopr/mnesia-backup-20231110-120000", []).'

# Fix permissions
sudo chown -R happyshopr:happyshopr /var/lib/happyshopr/mnesia

# Start the application
sudo systemctl start happyshopr
```

## Updating the Application

### Rolling Update

```bash
# Build new release (version 1.1.0)
rebar3 as prod release

# Create tarball
cd _build/prod/rel
tar -czf happyshopr-1.1.0.tar.gz happyshopr/

# Copy to server
scp happyshopr-1.1.0.tar.gz user@your-server:/tmp/

# On the server:
sudo systemctl stop happyshopr

# Backup current version
sudo cp -r /opt/happyshopr /opt/happyshopr.backup

# Extract new version
cd /opt/happyshopr
sudo tar -xzf /tmp/happyshopr-1.1.0.tar.gz --strip-components=1

# Restore custom configs (if needed)
sudo cp /opt/happyshopr.backup/releases/1.0.0/sys.config releases/1.1.0/
sudo cp /opt/happyshopr.backup/releases/1.0.0/vm.args releases/1.1.0/

# Fix ownership
sudo chown -R happyshopr:happyshopr /opt/happyshopr

# Start new version
sudo systemctl start happyshopr

# Verify
curl http://localhost:8080/health
```

### Hot Code Upgrade (Advanced)

Erlang supports hot code upgrades without downtime:

```bash
# Build appup file (requires manual creation)
# See: http://erlang.org/doc/design_principles/appup_cookbook.html

# Generate upgrade package
rebar3 as prod relup tar

# Copy to server and install
sudo -u happyshopr /opt/happyshopr/bin/happyshopr upgrade 1.1.0
```

## Troubleshooting

### Service Won't Start

```bash
# Check logs
sudo journalctl -u happyshopr -n 50

# Check permissions
ls -la /opt/happyshopr
ls -la /var/lib/happyshopr

# Try running manually
sudo -u happyshopr /opt/happyshopr/bin/happyshopr console
# Press Ctrl+C twice to exit
```

### Port Already in Use

```bash
# Find what's using port 8080
sudo lsof -i :8080

# Kill the process
sudo kill -9 <PID>
```

### Mnesia Schema Errors

```bash
# Reset Mnesia (WARNING: deletes all data)
sudo systemctl stop happyshopr
sudo rm -rf /var/lib/happyshopr/mnesia/*
sudo systemctl start happyshopr
```

### High Memory Usage

```bash
# Check Erlang memory
sudo -u happyshopr /opt/happyshopr/bin/happyshopr eval 'erlang:memory().'

# Check system memory
free -m

# Restart if needed
sudo systemctl restart happyshopr
```

### Can't Connect to Remote Console

```bash
# Check if node is running
sudo -u happyshopr /opt/happyshopr/bin/happyshopr ping

# Check cookie matches in vm.args
cat /opt/happyshopr/releases/1.0.0/vm.args

# Try with explicit cookie
sudo -u happyshopr /opt/happyshopr/bin/happyshopr remote_console -setcookie YOUR_COOKIE
```

## Security Best Practices

1. **Change default API keys** in `sys.config`
2. **Use HTTPS** via Nginx reverse proxy
3. **Restrict firewall** to only necessary ports
4. **Regular backups** of Mnesia database
5. **Monitor logs** for suspicious activity
6. **Keep Erlang updated** for security patches
7. **Use strong cookies** in `vm.args`
8. **Restrict SSH access** to server
9. **Consider API rate limiting** (add to Cowboy middleware)
10. **Use environment variables** for secrets (requires code changes)

## Performance Tuning

### Erlang VM Settings

Edit `vm.args`:

```
# Increase maximum ports
-env ERL_MAX_PORTS 8192

# Increase async threads
+A 64

# Enable kernel polling
+K true

# Set scheduler threads (number of CPU cores)
+S 4:4

# Set max ETS tables
+e 256000
```

### Mnesia Tuning

In Erlang shell:

```erlang
% Check table info
mnesia:table_info(shopping_item, all).

% Dump to disk interval (milliseconds)
mnesia:dump_log_write_threshold(50000).

% Fragmentation settings (for large tables)
mnesia:change_table_frag(shopping_item, {activate, []}).
```

## Scaling Considerations

### Multi-Node Setup (Future)

HappyShopr is built on Erlang/OTP and can be clustered:

1. Deploy multiple nodes with unique names
2. Configure Mnesia for `disc_copies` on all nodes
3. Connect nodes using `net_adm:ping/1`
4. Use a load balancer (HAProxy, Nginx) in front

### Database Considerations

- Mnesia is suitable for moderate load
- For very high traffic, consider external database (requires code changes)
- Regular backups become critical

## Support

For deployment issues:
- Check logs: `sudo journalctl -u happyshopr -f`
- GitHub Issues: <your-repo-url>/issues
- Documentation: README.md

## Appendix: Quick Reference

```bash
# Common commands
sudo systemctl status happyshopr    # Check status
sudo systemctl restart happyshopr   # Restart
sudo journalctl -u happyshopr -f    # View logs
curl http://localhost:8080/health   # Health check

# Mnesia backup
sudo cp -r /var/lib/happyshopr/mnesia /var/backups/happyshopr/

# Remote console
sudo -u happyshopr /opt/happyshopr/bin/happyshopr remote_console

# Check running processes
ps aux | grep beam.smp
```
