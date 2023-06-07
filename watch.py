#!/usr/bin/env python

from plumbum.cmd import kubectl, kube_capacity

from prettytable import PrettyTable, PLAIN_COLUMNS
from threading import Thread, Lock
import sys
import math
import json
import os
import shutil
import time
import re


DATA_DIRECTORY = "/Users/dgempesaw/opt/kubectl.el/data"
pod_metrics_cache = {}
pod_kubectl_cache = {}
table = PrettyTable()
table.set_style(PLAIN_COLUMNS)
table.align = "l"
table.right_padding_width = 2
update_pod_lock = Lock()


SORT_COLUMN = sys.argv[3]


def main():
    arg = sys.argv[1]

    print(sys.argv)
    print(DATA_DIRECTORY)
    shutil.rmtree(DATA_DIRECTORY)
    os.mkdir(DATA_DIRECTORY)
    resources = arg.split(",")

    Thread(target=poll_node_metrics).start()
    Thread(target=watch_nodes).start()

    for resource in resources:
        Thread(target=watch, args=[resource]).start()

        if is_pod(resource):
            Thread(target=poll_pod_metrics, args=[resource]).start()


def is_pod(resource):
    return resource in ["po", "pod", "pods"]


def is_all_namespaces():
    namespace = sys.argv[2]
    return namespace == "All Namespaces"


def update_pod_output(resource):
    with update_pod_lock:
        table.clear_rows()

        for pod_name in pod_kubectl_cache.keys():
            line_metrics = ["" for _ in range(6)]
            if pod_name in pod_metrics_cache:
                line_metrics = [
                    pod_metrics_cache[pod_name]["cpu"]["requests"],
                    pod_metrics_cache[pod_name]["cpu"]["limits"],
                    pod_metrics_cache[pod_name]["cpu"]["utilization"],
                    pod_metrics_cache[pod_name]["memory"]["requests"],
                    pod_metrics_cache[pod_name]["memory"]["limits"],
                    pod_metrics_cache[pod_name]["memory"]["utilization"],
                ]

            table.add_row([pod_name] + line_metrics + pod_kubectl_cache[pod_name][1:])

        with open(f"{DATA_DIRECTORY}/{resource}", "w") as f:
            f.seek(0)
            f.write(table.get_string())
            f.write("\n")
            f.truncate()


def watch(resource):
    command = ["get", resource, "--show-kind=true", "-owide"]
    if is_all_namespaces():
        command += ["--all-namespaces"]
    else:
        command += ["--watch"]
    p = kubectl[command].popen()

    if is_pod(resource) and not is_all_namespaces():
        headers = re.split("\\s{3,}", p.stdout.readline().decode("utf-8").strip())
        table.field_names = [
            "NAME",
            "CReq",
            "CLim",
            "CUse",
            "MReq",
            "MLim",
            "MUse",
        ] + headers[1:]
        table.sortby = SORT_COLUMN
        if SORT_COLUMN in SORT_FUNCTIONS:
            table.sort_key = SORT_FUNCTIONS[SORT_COLUMN]

        while True:
            line = re.split("\\s{3,}", p.stdout.readline().decode("utf-8").strip())
            name = line[0]

            pod_kubectl_cache[name] = line

            ready = line[1]
            status = line[2]

            if ready[0] == "0" and status == "Terminating":
                del pod_kubectl_cache[name]

            if p.poll() != None:
                break

            update_pod_output(resource)
    else:
        cache = {}
        with open(f"{DATA_DIRECTORY}/{resource}", "w") as f:
            while True:
                line = p.stdout.readline().decode("utf-8")
                if is_all_namespaces():
                    [_, name, *_] = re.split("\\s{3,}", line, 3)
                else:
                    name = line.split(" ")[0]
                cache[name] = line

                f.seek(0)
                f.write("".join(cache.values()))
                f.truncate()

                if p.poll() != None:
                    break


def poll_pod_metrics(resource):
    if is_all_namespaces():
        return

    namespace = sys.argv[2]
    metrics = json.loads(
        kube_capacity[
            f"--namespace {namespace} --util --pods --output json".split(" ")
        ]()
    )

    for node in metrics["nodes"]:
        if "pods" in node:
            for pod in node["pods"]:
                cpu_request = int(pod["cpu"]["requests"].rstrip("m"))
                cpu_util = int(pod["cpu"]["utilization"].rstrip("m"))

                memory_request = int(pod["memory"]["requests"].rstrip("Mi"))
                memory_util = int(pod["memory"]["utilization"].rstrip("Mi"))
                pod_metrics_cache[f"pod/{pod['name']}"] = {
                    "cpu": {
                        "requests": pod["cpu"]["requests"],
                        "limits": pod["cpu"]["limits"],
                        "utilization": f"{pod['cpu']['utilization']} ({math.floor(cpu_util / cpu_request * 100)}%)"
                        if cpu_request > 0
                        else "",
                    },
                    "memory": {
                        "requests": pod["memory"]["requests"],
                        "limits": pod["memory"]["limits"],
                        "utilization": f"{pod['memory']['utilization']} ({math.floor(memory_util / memory_request * 100)}%)"
                        if memory_request > 0
                        else "",
                    },
                }

    update_pod_output(resource)
    time.sleep(10)
    poll_pod_metrics(resource)


node_metrics_cache = {}
node_status_cache = {}
node_table = PrettyTable()
node_table.set_style(PLAIN_COLUMNS)
node_table.align = "l"
node_table.right_padding_width = 2
update_node_lock = Lock()


def poll_node_metrics():
    node_metrics = (kube_capacity["--util"]()).split("\n")[1:]
    for node_line in node_metrics:
        [name, *metrics] = re.split("\\s{3,}", node_line.strip())
        node_metrics_cache[f"node/{name}"] = metrics

    update_node_output()
    time.sleep(10)
    poll_node_metrics()


def watch_nodes():
    p = kubectl[
        "get",
        "nodes",
        "--watch",
        "--show-kind=true",
        "--label-columns=node.kubernetes.io/instance-type,topology.kubernetes.io/zone,app.pagerduty.com/taec-node-group-name,app.pagerduty.com/repo",
    ].popen()

    headers = re.split("\\s{3,}", p.stdout.readline().decode("utf-8").strip())
    node_table.field_names = [
        "NAME",
        "CReq",
        "CLim",
        "CUse",
        "MReq",
        "MLim",
        "MUse",
    ] + headers[1:]
    node_table.sortby = SORT_COLUMN
    if SORT_COLUMN in SORT_FUNCTIONS:
        node_table.sort_key = SORT_FUNCTIONS[SORT_COLUMN]

    while True:
        [name, *status] = re.split(
            "\\s{3,}", p.stdout.readline().decode("utf-8").strip()
        )
        node_status_cache[name] = status

        if p.poll() != None:
            break

        update_node_output()


def update_node_output():
    with update_node_lock:
        node_table.clear_rows()

        for node_name in node_status_cache.keys():
            metrics = ["" for _ in range(6)]
            if node_name in node_metrics_cache:
                metrics = node_metrics_cache[node_name]
            status = node_status_cache[node_name]

            node_table.add_row([node_name] + metrics + status)

        with open(f"{DATA_DIRECTORY}/kcnodes", "w") as f:
            f.seek(0)
            f.write(node_table.get_string())
            f.write("\n")
            f.truncate()


def sort_age(vals):
    # first item is the sort_by column, the rest of the items is the entire row
    # data
    matches = re.search(
        "(?:(?P<days>\\d+)d)?(?:(?P<hours>\\d+)h)?(?:(?P<minutes>\\d+)m)?(?:(?P<seconds>\\d+)s)?",
        vals[0],
    )
    if matches:
        return (
            int(matches.group("days") or 0) * 24 * 60 * 60
            + int(matches.group("hours") or 0) * 60 * 60
            + int(matches.group("minutes") or 0) * 60
            + int(matches.group("seconds") or 0)
        )
    return 0


def sort_with_percent(vals):
    try:
        return int(re.sub("\\D", "", vals[0].split(" ")[0]))
    except:
        return 0


SORT_FUNCTIONS = {
    "AGE": sort_age,
    "CReq": sort_with_percent,
    "CLim": sort_with_percent,
    "CUse": sort_with_percent,
    "MReq": sort_with_percent,
    "MLim": sort_with_percent,
    "MUse": sort_with_percent,
}


main()
