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


def update_pod_output(resource):
    with update_pod_lock:
        table.clear_rows()

        for pod_name in pod_kubectl_cache.keys():
            line_metrics = ["" for _ in range(8)]
            if pod_name in pod_metrics_cache:
                line_metrics = [
                    pod_metrics_cache[pod_name]["cpu"]["utilization"],
                    pod_metrics_cache[pod_name]["cpu"]["utilization_percent"],
                    pod_metrics_cache[pod_name]["cpu"]["requests"],
                    pod_metrics_cache[pod_name]["cpu"]["limits"],
                    pod_metrics_cache[pod_name]["memory"]["utilization"],
                    pod_metrics_cache[pod_name]["memory"]["utilization_percent"],
                    pod_metrics_cache[pod_name]["memory"]["requests"],
                    pod_metrics_cache[pod_name]["memory"]["limits"],
                ]

            table.add_row([pod_name] + line_metrics + pod_kubectl_cache[pod_name][1:])

        with open(f"{DATA_DIRECTORY}/{resource}", "w") as f:
            f.seek(0)
            f.write(table.get_string())
            f.write("\n")
            f.truncate()


def watch(resource):
    p = kubectl["get", resource, "--watch", "--show-kind=true", "-owide"].popen()

    if is_pod(resource):
        headers = re.split("\\s{3,}", p.stdout.readline().decode("utf-8").strip())
        table.field_names = [
            "NAME",
            "CUse",
            "CPer",
            "CReq",
            "CLim",
            "MUse",
            "MPer",
            "MReq",
            "MLim",
        ] + headers[1:]
        table.sortby = "NAME"

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
                name = line.split(" ")[0]
                cache[name] = line

                f.seek(0)
                f.write("".join(cache.values()))
                f.truncate()

                if p.poll() != None:
                    break


def poll_pod_metrics(resource):
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
                        "utilization": pod["cpu"]["utilization"],
                        "utilization_percent": f"{math.floor(cpu_util / cpu_request * 100)}%"
                        if cpu_request > 0
                        else "",
                    },
                    "memory": {
                        "requests": pod["memory"]["requests"],
                        "limits": pod["memory"]["limits"],
                        "utilization": pod["memory"]["utilization"],
                        "utilization_percent": f"{math.floor(memory_util / memory_request * 100)}%"
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
    p = kubectl["get", "nodes", "--watch", "--show-kind=true", "-owide"].popen()

    headers = re.split("\\s{3,}", p.stdout.readline().decode("utf-8").strip())
    node_table.field_names = [
        "NAME",
        "CPU REQUESTS",
        "CPU LIMITS",
        "CPU UTIL",
        "MEMORY REQUESTS",
        "MEMORY LIMITS",
        "MEMORY UTIL",
    ] + headers[1:]
    node_table.sortby = "NAME"

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


main()
