# `dev/gcp`

GCP / Kubernetes / Terraform 用の開発環境です。

## Services

- `gcp`
- `k8s`
- `terraform`

3サービスはそれぞれ別イメージを使い、`~/.config/gcloud` と `~/.kube` を named volume で共有します。

## Usage

```bash
cd dev/gcp
cp .env.example .env
make pull
make login
make auth
make bash
make bash SERVICE=k8s
make bash SERVICE=terraform
```

## Environment Variables

`.env.example` を `.env` にコピーして使います。各サービスは `environment:` ではなく `env_file` として `.env` を読み込みます。

- `CLOUDSDK_CORE_PROJECT`
- `CLOUDSDK_CONFIG`
- `CLOUDSDK_COMPUTE_REGION`
- `CLOUDSDK_COMPUTE_ZONE`
- `GOOGLE_APPLICATION_CREDENTIALS`
- `GOOGLE_CLOUD_PROJECT`
- `KUBECONFIG`
- `TF_VAR_project_id`
