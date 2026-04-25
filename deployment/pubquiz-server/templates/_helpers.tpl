{{- define "pubquiz-server.labels" -}}
app.kubernetes.io/name: pubquiz-server
app.kubernetes.io/instance: {{ .Release.Name }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- end }}

{{- define "pubquiz-server.selectorLabels" -}}
app.kubernetes.io/name: pubquiz-server
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end }}

{{- define "pubquiz-server.databaseHost" -}}
{{- .Release.Name }}-postgres
{{- end }}
