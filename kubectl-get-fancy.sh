#! /usr/bin/env bash


set -o pipefail
namespace=$1
kube_opts=$2
resources_before_pods=$3
resources_after_pods=$4

kube_capacity_output=$(mktemp)
kube_capacity_summary=$(mktemp)
kubectl_get_pods_output=$(mktemp)
kubectl_get_before_resources_output=$(mktemp)
kubectl_get_after_resources_output=$(mktemp)

pk ns $namespace > /dev/null

kube-capacity --util | head -n 2 > $kube_capacity_summary 2>&1 &

kube-capacity --namespace $namespace --util --pods -ojson | jq -r '.nodes[].pods[]?' | jq -s 'map(if .cpu.limits == "0m" then . else .cpu.utilizationPercent = (((.cpu.utilization | rtrimstr("m") | tonumber) / (.cpu.limits | rtrimstr("m") | tonumber)) * 100 | floor | tostring) + "%" end) | map(if .memory.limits == "0Mi" then . else .memory.utilizationPercent = (((.memory.utilization | rtrimstr("Mi") | tonumber) / (.memory.limits | rtrimstr("Mi") | tonumber)) * 100 | floor | tostring) + "%" end)' > $kube_capacity_output 2>&1 &

kubectl $kube_opts get pods --no-headers -owide 2>&1 | grep -v Got\ empty | jq --raw-input 'split("\n") | map(splits("  +")) | { "name": .[0], "ready": .[1], "status": .[2], "restarts": .[3], "age": .[4], "ip": .[5], "node": .[6], "nominated node": .[7], "readiness gates": .[8] }' | jq -s . > $kubectl_get_pods_output 2>&1 &

prefix=""
if [[ "$resources_before_pods" != "" ]]; then
    prefix="pod/"
    kubectl $kube_opts get $resources_before_pods 2>&1 | grep -v Got\ empty > $kubectl_get_before_resources_output 2>&1 &
fi

if [[ "$resources_after_pods" != "" ]]; then
    prefix="pod/"
    kubectl $kube_opts get $resources_after_pods 2>&1 | grep -v Got\ empty > $kubectl_get_after_resources_output 2>&1 &
fi

wait

# cat $kube_capacity_summary
# echo

cat $kubectl_get_before_resources_output | grep -v "No resources"
if [[ "$resources_before_pods" != "" ]]; then
    echo
fi

jq -s '[ .[0] + .[1] | group_by(.name)[] | add ]' $kubectl_get_pods_output $kube_capacity_output | jq -r '["NAME", "CUse", "CPer", "CReq", "CLim", "MUse", "MPer", "MReq", "MLim", "READY", "STATUS", "RESTARTS", "AGE", "IP", "NODE"], (.[] | ["'"$prefix"'" + .name, .cpu.utilization // " ", .cpu.utilizationPercent // " ", .cpu.requests // " ", .cpu.limits // " ", .memory.utilization // " ", .memory.utilizationPercent // " ", .memory.requests // " ", .memory.limits // " ", .ready, .status, .restarts, .age, .ip, .node ]) | @tsv' | column -ts $'\t'

if [[ "$resources_after_pods" != "" ]]; then
    echo
fi
cat $kubectl_get_after_resources_output | grep -v "No resources"

rm $kube_capacity_output
rm $kube_capacity_summary
rm $kubectl_get_pods_output
rm $kubectl_get_before_resources_output
rm $kubectl_get_after_resources_output
