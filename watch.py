#!/usr/bin/env python

import json
import math
import os
import re
import sys
import time
from pathlib import Path
from threading import Lock, Thread

from emacs import EmacsClient
from plumbum.cmd import kube_capacity, kubectl, sort, uniq
from prettytable import PLAIN_COLUMNS, PrettyTable

EMACS_SERVER_SOCKET_DIR = Path(os.environ.get("EMACS_SERVER_SOCKET_DIR"))
KUBECTL_DIRECTORY = "/Users/gempesaw/opt/kubectl.el"
DATA_DIRECTORY = "/private/var/tmp/kubectl-data"
pod_metrics_cache = {}
pod_kubectl_cache = {}
table = PrettyTable()
table.set_style(PLAIN_COLUMNS)
table.align = "l"
table.right_padding_width = 2
update_pod_lock = Lock()


SORT_COLUMN = sys.argv[3]
REVERSE_SORT = False if SORT_COLUMN == "AGE" else True
GREP_NEEDLE = sys.argv[4]
IS_GREP = GREP_NEEDLE != "-"


def make_emacs_client():
    server_socket = next(EMACS_SERVER_SOCKET_DIR.glob("*"))
    return EmacsClient(server=server_socket)


emacs = make_emacs_client()


def main():
    os.chdir(KUBECTL_DIRECTORY)
    arg = sys.argv[1]

    print(sys.argv)
    print(DATA_DIRECTORY)
    Path(DATA_DIRECTORY).mkdir(parents=True, exist_ok=True)
    resources = arg.split(",")

    # Thread(target=poll_node_metrics).start()
    Thread(target=poll_podcount).start()
    Thread(target=watch_nodes).start()
    Thread(target=refresh_nodes).start()

    for resource in resources:
        Thread(target=watch, args=[resource]).start()

        # if is_pod(resource):
        #     Thread(target=poll_pod_metrics, args=[resource]).start()


def is_pod(resource):
    return resource in ["po", "pod", "pods"]


def is_all_namespaces():
    namespace = sys.argv[2]
    return namespace == "All Namespaces"


def write_output_to_emacs_buffer(resource, contents):
    try:
        buffer = f" kubectl--resource-buffer-{resource}"
        emacs.eval(
            f'(kubectl--write-buffer-contents "{buffer}" "{contents.replace('"', '\\"')}")'
        )
    except:
        print("error writing to emacs")


def update_pod_output(resource):
    with update_pod_lock:
        table.clear_rows()

        keys = pod_kubectl_cache.keys()
        total = len(keys)
        for pod_name in keys:
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

            data = [pod_name] + line_metrics + pod_kubectl_cache[pod_name][1:]
            if total > 20 and IS_GREP:
                if GREP_NEEDLE in " ".join(data):
                    table.add_row(data)
            else:
                table.add_row(data)

        contents = table.get_string(start=0, end=20).replace(
            f"NAME {len(str(total)) * " "}", f"NAME {total}"
        )

        write_output_to_emacs_buffer(resource, contents)


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

    if is_pod(resource):
        command += ["--field-selector=status.phase!=Completed"]
    else:
        if "--watch" in command:
            command += ["--output-watch-events=true"]

    print(" ".join(command))
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

        while True:
            line = p.stdout.readline().decode("utf-8").strip()
            if line == "":
                time.sleep(1)
                continue
            else:
                break

        headers = re.split("\\s{3,}", line)
        header_start_positions = re.finditer("\\s{3}[A-Z]", line)
        headers_columns = len(headers)

        # ignore the first column EVENT
        resource_table.field_names = headers[1:]
        sort_column = get_sort_column(resource_table)
        resource_table.sortby = sort_column
        if sort_column in SORT_FUNCTIONS:
            resource_table.sort_key = SORT_FUNCTIONS[sort_column]

        while True:
            line_text = p.stdout.readline().decode("utf-8").strip()
            for start_position_span in header_start_positions:
                column = start_position_span.span(0)[0] + 3
                if line_text[column] == " ":
                    line_text = line_text[:column] + "." + line_text[(column + 1) :]

            [event, *line] = re.split("\\s{3,}", line_text)

            if is_all_namespaces():
                # event, namespace, name, ...
                name = line[1]
            else:
                # event, name
                name = line[0]

            line_columns = len(line)
            # if line_columns < headers_columns:
            #     if "nodeclaim" in resource.lower() and len(line) > 4:
            #         # sometimes the NODE column is empty and it throws off the
            #         # rest of the columns, particularly for sorting nodeclaims
            #         # by age
            #         if "ip-" not in line[4] and "i-" not in line[4]:
            #             line = line[0:4] + [""] + line[4:]
            #             line_columns = len(line)
            #     missing = headers_columns - line_columns - 1
            #     buffer = ["" for _ in range(missing)]
            #     line = line[0:-1] + buffer + [line[-1]]

            if event in ["ADDED", "MODIFIED"]:
                cache[name] = line

            if event == "DELETED":
                del cache[name]

            resource_table.clear_rows()

            total = len(cache.keys())
            for name in cache.keys():
                if total > 20 and IS_GREP:
                    data = [name] + cache[name][1:]
                    if GREP_NEEDLE in " ".join(data):
                        resource_table.add_row(data)
                else:
                    resource_table.add_row([name] + cache[name][1:])

            contents = resource_table.get_string(
                start=0, end=20, reversesort=REVERSE_SORT
            ).replace(f"NAME {len(str(total)) * " "}", f"NAME {total}")

            write_output_to_emacs_buffer(resource, contents)

            if p.poll() != None:
                break


def poll_pod_metrics(resource):
    if is_all_namespaces():
        return

    namespace = sys.argv[2]
    metrics = json.loads(
        kube_capacity[
            f"--namespace {namespace} --util --pods --output json".split(" ")
            # f"--namespace {namespace} --pods --output json".split(" ")
        ]()
    )

    for node in metrics["nodes"]:
        if "pods" in node:
            for pod in node["pods"]:
                pod["cpu"]["utilization"] = pod["cpu"].get("utilization", "0m")
                pod["memory"]["utilization"] = pod["memory"].get("utilization", "0Mi")
                cpu_request = int(pod["cpu"]["requests"].rstrip("m"))
                cpu_util = int(pod["cpu"]["utilization"].rstrip("m"))

                memory_request = int(pod["memory"]["requests"].rstrip("Mi"))
                memory_util = int(pod["memory"]["utilization"].rstrip("Mi"))

                pod_metrics_cache[f"pod/{pod['name']}"] = {
                    "cpu": {
                        "requests": pod["cpu"]["requests"],
                        "limits": pod["cpu"]["limits"],
                        "utilization": f"{percent_as_lines(f"{math.floor(cpu_util / cpu_request * 100)}%")} {pod['cpu']['utilization']}"
                        if cpu_request > 0 and cpu_util > 0
                        else "",
                    },
                    "memory": {
                        "requests": pod["memory"]["requests"],
                        "limits": pod["memory"]["limits"],
                        "utilization": f"{percent_as_lines(f"({math.floor(memory_util / memory_request * 100)}%)")} {pod['memory']['utilization']}"
                        if memory_request > 0 and memory_util > 0
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
        kubectl[
            "get pods -A --field-selector=status.phase==Running -o custom-columns=node:.spec.nodeName".split(
                " "
            )
        ]
        | sort
        | uniq["-c"]
        | sort["-n"]
    )

    for pc in podcount().split("\n"):
        if pc.strip() != "":
            [count, node_name] = re.split(r"\s+", pc.strip())
            node_podcount_cache[f"node/{node_name}"] = count

    update_node_output()
    time.sleep(15)
    poll_podcount()


def refresh_nodes():
    update_node_output()
    time.sleep(15)
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
    time.sleep(60)
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
    util = int(parts.group(1))
    if util == 0:
        util = 1
    return math.ceil(util / 20) * "|"


def watch_nodes():
    p = kubectl[
        "get",
        "nodes",
        "--watch",
        "--show-kind=true",
        "--label-columns=topology.kubernetes.io/zone,node.kubernetes.io/instance-type,karpenter.sh/nodepool,karpenter.sh/capacity-type",
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

        namespace = sys.argv[2]
        if namespace == "buildbarn":
            if "buildbarn" in " ".join(status):
                node_status_cache[name] = status
        else:
            node_status_cache[name] = status

        if p.poll() != None:
            break


def update_node_output():
    with update_node_lock:
        node_table.clear_rows()

        total = len(node_status_cache.keys())
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

            if total > 20 and IS_GREP and GREP_NEEDLE != "karpenter":
                if GREP_NEEDLE in " ".join(row):
                    node_table.add_row(row)
            else:
                node_table.add_row(row)

        contents = node_table.get_string()
        write_output_to_emacs_buffer("kcnodes", contents)


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
