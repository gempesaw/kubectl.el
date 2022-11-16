#! /usr/bin/env bash

set -o pipefail

namespace=$1
kube_opts=$2
resources_before_pods=$3
resources_after_pods=$4

kube_capacity_output=$(mktemp)
kubectl_get_pods_output=$(mktemp)
kubectl_get_before_resources_output=$(mktemp)
kubectl_get_after_resources_output=$(mktemp)

kube-capacity --namespace $namespace --util --pods -ojson | jq -r '.nodes[].pods[]' | jq -s . > $kube_capacity_output &

kubectl $kube_opts get pods --no-headers -owide | jq --raw-input 'split("\n") | map(splits(" +")) | { "name": .[0], "ready": .[1], "status": .[2], "restarts": .[3], "age": .[4], "ip": .[5], "node": .[6], "nominated node": .[7], "readiness gates": .[8] }' | jq -s . > $kubectl_get_pods_output &


prefix=""
if [[ "$resources_before_pods" != "" ]]; then
    prefix="pod/"
    kubectl $kube_opts get $resources_before_pods > $kubectl_get_before_resources_output &
fi

if [[ "$resources_after_pods" != "" ]]; then
    prefix="pod/"
    kubectl $kube_opts get $resources_after_pods > $kubectl_get_after_resources_output &
fi

wait

cat $kubectl_get_before_resources_output
if [[ "$resources_before_pods" != "" ]]; then
    echo
fi

jq -s '[ .[0] + .[1] | group_by(.name)[] | select(length > 1) | add ]' $kube_capacity_output $kubectl_get_pods_output | jq -r '["NAME", "CUse", "CPer", "CReq", "CLim", "MUse", "MPer", "MReq", "MLim", "READY", "STATUS", "RESTARTS", "AGE", "IP", "NODE"], (.[] | ["'"$prefix"'" + .name, .cpu.utilization, .cpu.utilizationPercent, .cpu.requests, .cpu.limits, .memory.utilization, .memory.utilizationPercent, .memory.requests, .memory.limits, .ready, .status, .restarts, .age, .ip, .node ]) | @tsv' | column -t

if [[ "$resources_after_pods" != "" ]]; then
    echo
fi
cat $kubectl_get_after_resources_output

rm $kube_capacity_output
rm $kubectl_get_pods_output
rm $kubectl_get_before_resources_output
rm $kubectl_get_after_resources_output
