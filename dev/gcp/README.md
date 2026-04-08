# `dev/gcp`

GCP / Kubernetes / Terraform 用の開発環境です。

## Services

- `gcp`
- `proxy`
- `k8s`
- `mysql`
- `terraform`

5サービスはそれぞれ別イメージを使い、`~/.config/gcloud` と `~/.kube` を named volume で共有します。`proxy` は Cloud SQL Auth Proxy を起動し、`mysql` はその `proxy:3306` に接続するためのクライアント用サービスです。

## Usage

```bash
cd dev/gcp
cp .env.example .env
make pull
make login
make auth
make bash
make up
make proxy-up
make mysql
make proxy-down
make bash SERVICE=mysql
mysql -h proxy -P 3306 -u <user> -p
make bash SERVICE=k8s
make bash SERVICE=terraform
```

## Environment Variables

`.env.example` を `.env` にコピーして使います。各サービスは `environment:` ではなく `env_file` として `.env` を読み込みます。

- `CLOUDSDK_CORE_PROJECT`
- `CLOUDSDK_CONFIG`
- `CLOUDSDK_COMPUTE_REGION`
- `CLOUDSDK_COMPUTE_ZONE`
- `CLOUDSQL_CONNECTION_NAME`
- `CLOUDSQL_PROXY_CREDENTIALS_FILE`
- `GOOGLE_APPLICATION_CREDENTIALS`
- `GOOGLE_CLOUD_PROJECT`
- `KUBECONFIG`
- `MYSQL_DATABASE`
- `MYSQL_HOST`
- `MYSQL_PORT`
- `MYSQL_PWD`
- `MYSQL_USER`
- `TF_VAR_project_id`
