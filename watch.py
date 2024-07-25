#!/usr/bin/env python

import json
import math
import os
import re
import shutil
import sys
import time
from threading import Lock, Thread

from plumbum.cmd import kube_capacity, kubectl, sort, uniq
from prettytable import PLAIN_COLUMNS, PrettyTable

KUBECTL_DIRECTORY = "/Users/dgempesaw/opt/kubectl.el"
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
    os.chdir(KUBECTL_DIRECTORY)
    arg = sys.argv[1]

    print(sys.argv)
    print(DATA_DIRECTORY)
    shutil.rmtree(DATA_DIRECTORY)
    os.mkdir(DATA_DIRECTORY)
    resources = arg.split(",")

    Thread(target=poll_node_metrics).start()
    Thread(target=poll_podcount).start()
    Thread(target=watch_nodes).start()
    Thread(target=refresh_nodes).start()

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


def get_sort_column(table, default_sort_column="NAME"):
    headers = table.field_names
    if SORT_COLUMN in headers:
        return SORT_COLUMN

    return default_sort_column


def watch(resource):
    command = ["get", resource, "--show-kind=true", "-owide"]
    if is_all_namespaces():
        command += ["--all-namespaces"]
    else:
        command += ["--watch"]

    if not is_pod(resource):
        command += ["--output-watch-events=true"]

    print(command)
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
        sort_column = get_sort_column(table)
        table.sortby = sort_column
        if sort_column in SORT_FUNCTIONS:
            table.sort_key = SORT_FUNCTIONS[sort_column]

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
        resource_table = PrettyTable()
        resource_table.set_style(PLAIN_COLUMNS)
        resource_table.align = "l"
        resource_table.right_padding_width = 2

        with open(f"{DATA_DIRECTORY}/{resource}", "w") as f:
            while True:
                line = p.stdout.readline().decode("utf-8").strip()
                if line == "":
                    time.sleep(1)
                    continue
                else:
                    break

            headers = re.split("\\s{3,}", line)
            headers_columns = len(headers)

            # ignore the first column EVENT
            resource_table.field_names = headers[1:]
            sort_column = get_sort_column(resource_table)
            resource_table.sortby = sort_column
            if sort_column in SORT_FUNCTIONS:
                resource_table.sort_key = SORT_FUNCTIONS[sort_column]

            while True:
                [event, *line] = re.split(
                    "\\s{3,}", p.stdout.readline().decode("utf-8").strip()
                )
                if is_all_namespaces():
                    # event, namespace, name, ...
                    name = line[1]
                else:
                    # event, name
                    name = line[0]

                line_columns = len(line)
                if line_columns < headers_columns:
                    missing = headers_columns - line_columns - 1
                    buffer = ["" for _ in range(missing)]
                    line = line[0:-1] + buffer + [line[-1]]

                if event in ["ADDED", "MODIFIED"]:
                    cache[name] = line

                if event == "DELETED":
                    del cache[name]

                resource_table.clear_rows()

                for name in cache.keys():
                    resource_table.add_row([name] + cache[name][1:])

                f.seek(0)
                f.write(resource_table.get_string())
                f.write("\n")
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
node_podcount_cache = {}
node_status_cache = {}
node_table = PrettyTable()
node_table.set_style(PLAIN_COLUMNS)
node_table.align = "l"
node_table.right_padding_width = 2
update_node_lock = Lock()


def poll_podcount():
    podcount = (
        kubectl["get pods -A -o custom-columns=node:.spec.nodeName".split(" ")]
        | sort
        | uniq["-c"]
        | sort["-n"]
    )

    for pc in podcount().split("\n"):
        if pc.strip() != "":
            [count, node_name] = re.split(r"\s+", pc.strip())
            node_podcount_cache[f"node/{node_name}"] = count

    update_node_output()
    time.sleep(30)
    poll_podcount()


def refresh_nodes():
    update_node_output()
    time.sleep(10)
    refresh_nodes()


def poll_node_metrics():
    node_metrics = (kube_capacity["--util"]()).split("\n")[1:]
    for node_line in node_metrics:
        # skip the limit ones, we don't care
        if node_line.strip() != "":
            [name, creq, _, cuse, mreq, _, muse] = re.split(
                "\\s{3,}", node_line.strip()
            )
            metrics = [make_metric_pretty(m) for m in [creq, cuse, mreq, muse]]
            node_metrics_cache[f"node/{name}"] = metrics

    update_node_output()
    time.sleep(30)
    poll_node_metrics()


def make_metric_pretty(metric):
    resource, percent = metric.split(" ")
    if "m" in metric:
        return f"{percent_as_lines(percent)} {round_cpu(resource)}"
    else:
        return f"{percent_as_lines(percent)} {round_mem(resource)}"


def round_cpu(resource):
    parts = re.search(r"(\d+)([a-z]+)", resource)
    return round(int(parts.group(1)) / 1000, 1)


def round_mem(resource):
    parts = re.search(r"(\d+)Mi", resource)
    return f"{round(int(parts.group(1)) / 1000, 1)}Gi"


def percent_as_lines(percent):
    parts = re.search(r"(\d+)%", percent)
    return math.ceil(int(parts.group(1)) / 20) * "|"


def watch_nodes():
    p = kubectl[
        "get",
        "nodes",
        "--watch",
        "--show-kind=true",
        "--label-columns=topology.kubernetes.io/zone,node.kubernetes.io/instance-type,karpenter.sh/capacity-type",
    ].popen()

    headers = re.split("\\s{3,}", p.stdout.readline().decode("utf-8").strip())
    node_table.field_names = [
        "NAME",
        "Pods",
        "CReq",
        "CUse",
        "MReq",
        "MUse",
    ] + headers[1:]
    sort_column = get_sort_column(node_table)
    node_table.sortby = sort_column
    if sort_column in SORT_FUNCTIONS:
        node_table.sort_key = SORT_FUNCTIONS[sort_column]

    while True:
        [name, *status] = re.split(
            "\\s{3,}", p.stdout.readline().decode("utf-8").strip()
        )
        node_status_cache[name] = status

        if p.poll() != None:
            break


def update_node_output():
    with update_node_lock:
        node_table.clear_rows()

        for node_name in node_status_cache.keys():
            if "fargate" in node_name:
                continue

            pods = ""
            if node_name in node_podcount_cache:
                pods = node_podcount_cache[node_name]

            metrics = ["" for _ in range(4)]
            if node_name in node_metrics_cache:
                for index, metric in enumerate(node_metrics_cache[node_name]):
                    metrics[index] = metric

            status = node_status_cache[node_name]

            row = [node_name, pods] + metrics + status
            header_length = len(node_table.field_names)
            while header_length > len(row):
                row = row + [""]

            node_table.add_row(row)

            string = node_table.get_string()
            with open(f"{DATA_DIRECTORY}/kcnodes", "w") as f:
                f.seek(0)
                f.write(string)
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
