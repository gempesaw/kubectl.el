#!/usr/bin/env python

import asyncio
import json
import math
import os
import re
import sys
import time
from pathlib import Path

from emacs import EmacsClient
from plumbum.cmd import jq, kube_capacity, kubectl
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

SORT_COLUMN = sys.argv[3]
REVERSE_SORT = False if SORT_COLUMN == "AGE" else True
GREP_NEEDLE = sys.argv[4]
IS_GREP = GREP_NEEDLE != "-"

# Debouncing: track last update time for each resource
last_update_time = {}
pending_writes = {}  # Track pending scheduled writes
UPDATE_DEBOUNCE_MS = 100  # Debounce window in milliseconds


def make_emacs_client():
    server_socket = next(EMACS_SERVER_SOCKET_DIR.glob("*"))
    return EmacsClient(server=server_socket)


emacs = make_emacs_client()


async def main():
    os.chdir(KUBECTL_DIRECTORY)
    arg = sys.argv[1]

    print(sys.argv)
    print(DATA_DIRECTORY)
    Path(DATA_DIRECTORY).mkdir(parents=True, exist_ok=True)
    resources = arg.split(",")

    # Create all tasks concurrently
    tasks = [
        asyncio.create_task(poll_node_metrics()),
        asyncio.create_task(poll_podcount()),
        asyncio.create_task(watch_nodes()),
        asyncio.create_task(refresh_nodes()),
    ]

    for resource in resources:
        tasks.append(asyncio.create_task(watch(resource)))

        if is_pod(resource):
            tasks.append(asyncio.create_task(poll_pod_metrics(resource)))

    # Wait for all tasks (they run indefinitely)
    await asyncio.gather(*tasks)


def is_pod(resource):
    return resource in ["po", "pod", "pods"]


def is_all_namespaces():
    namespace = sys.argv[2]
    return namespace == "All Namespaces"


async def _do_write_to_emacs(resource, contents):
    """Actually write to Emacs buffer."""
    try:
        buffer = f" kubectl--resource-buffer-{resource}"
        emacs.eval(
            f'(kubectl--write-buffer-contents "{buffer}" "{contents.replace('"', '\\"')}")'
        )
        last_update_time[resource] = time.time()
        # print(f"Wrote {resource} to Emacs")
    except Exception as e:
        print(f"error writing {resource} to emacs: {e}")


def write_output_to_emacs_buffer(resource, contents):
    """Write to Emacs buffer with debouncing and scheduled callbacks."""
    current_time = time.time()
    last_time = last_update_time.get(resource, 0)
    time_since_last = current_time - last_time

    # If enough time has passed, write immediately
    if time_since_last >= UPDATE_DEBOUNCE_MS / 1000:
        # Cancel any pending write for this resource
        if resource in pending_writes:
            pending_writes[resource].cancel()
            del pending_writes[resource]

        # Write immediately (schedule as a task)
        asyncio.create_task(_do_write_to_emacs(resource, contents))
        return

    # Cancel any existing pending write
    if resource in pending_writes:
        pending_writes[resource].cancel()

    # Schedule new write after the debounce period
    delay = (UPDATE_DEBOUNCE_MS / 1000) - time_since_last

    async def delayed_write():
        await asyncio.sleep(delay)
        await _do_write_to_emacs(resource, contents)
        if resource in pending_writes:
            del pending_writes[resource]

    pending_writes[resource] = asyncio.create_task(delayed_write())


def update_pod_output(resource):
    """Update pod output - no lock needed with asyncio single-threaded execution."""
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
        if IS_GREP:
            if GREP_NEEDLE in " ".join(data):
                table.add_row(data)
        else:
            table.add_row(data)

    contents = table.get_string(start=0, end=20).replace(
        f"NAME {len(str(total)) * ' '}", f"NAME {total}"
    )

    write_output_to_emacs_buffer(resource, contents)


def get_sort_column(table, default_sort_column="NAME"):
    headers = table.field_names
    if SORT_COLUMN in headers:
        return SORT_COLUMN

    return default_sort_column


async def watch(resource):
    """Watch a kubectl resource asynchronously."""
    try:
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

        print(f"{resource}: {' '.join(command)}")
        # Use asyncio subprocess for non-blocking I/O
        p = await asyncio.create_subprocess_exec(
            "kubectl",
            *command,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )
        print(f"{resource}: process started")

        if is_pod(resource) and not is_all_namespaces():
            line_bytes = await p.stdout.readline()
            headers = re.split("\\s{3,}", line_bytes.decode("utf-8").strip())
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
                line = await p.stdout.readline()
                if not line:
                    break

                line = re.split("\\s{3,}", line.decode("utf-8").strip())
                name = line[0]
                pod_kubectl_cache[name] = line

                ready = line[1]
                status = line[2]
                if ready[0] == "0" and status == "Terminating":
                    del pod_kubectl_cache[name]

                if p.returncode is not None:
                    break

                update_pod_output(resource)
        else:
            cache = {}
            resource_table = PrettyTable()
            resource_table.set_style(PLAIN_COLUMNS)
            resource_table.align = "l"
            resource_table.right_padding_width = 2

            while True:
                line_bytes = await p.stdout.readline()
                if not line_bytes:
                    await asyncio.sleep(0.1)
                    continue

                line = line_bytes.decode("utf-8").strip()
                if line == "":
                    await asyncio.sleep(0.1)
                    continue
                else:
                    break

            header_start_positions = list(re.finditer("\\s{3}[A-Z]", line))
            headers = re.split("\\s{3,}", line)
            headers_columns = len(headers)

            # ignore the first column EVENT
            resource_table.field_names = headers[1:]
            sort_column = get_sort_column(resource_table)
            resource_table.sortby = sort_column
            if sort_column in SORT_FUNCTIONS:
                resource_table.sort_key = SORT_FUNCTIONS[sort_column]

            while True:
                [event, *line] = await add_placeholders_async(header_start_positions, p)

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

                total = len(cache.keys())
                for name in cache.keys():
                    if IS_GREP:
                        data = [name] + cache[name][1:]
                        if GREP_NEEDLE in " ".join(data):
                            resource_table.add_row(data)
                    else:
                        resource_table.add_row([name] + cache[name][1:])

                contents = resource_table.get_string(
                    start=0, end=20, reversesort=REVERSE_SORT
                ).replace(f"NAME {len(str(total)) * ' '}", f"NAME {total}")

                write_output_to_emacs_buffer(resource, contents)

                if p.returncode is not None:
                    break
    except Exception as e:
        print(f"ERROR in watch({resource}): {e}")
        import traceback

        traceback.print_exc()


async def add_placeholders_async(header_start_positions, process):
    """Async version of add_placeholders for asyncio subprocess."""
    line_bytes = await process.stdout.readline()
    line_text = line_bytes.decode("utf-8").strip()

    # If we don't have header positions, fall back to original logic
    if not header_start_positions:
        return re.split("\\s{3,}", line_text)

    # Use a more robust approach: reconstruct the header line to find column positions
    # This assumes we can reconstruct the header from the positions we found
    headers = []
    positions = []

    for match in header_start_positions:
        pos = match.span(0)[0] + 3  # Skip the 3 spaces before header
        positions.append(pos)

    # Ensure line is long enough to check all columns
    if len(line_text) < max(positions) if positions else 0:
        line_text = line_text.ljust(max(positions) + 20)

    # Convert line to list for easier manipulation
    line_chars = list(line_text)

    # For each column position (except the first), check if it's empty
    for pos in positions:
        if pos < len(line_chars):
            # Find the end of this column
            next_pos = None
            for p in positions:
                if p > pos:
                    next_pos = p
                    break

            end_pos = next_pos if next_pos else len(line_chars)

            # Check if the column content is all spaces
            column_content = "".join(line_chars[pos:end_pos]).strip()
            if not column_content:
                # Place a subtle dash at the column start position
                if pos < len(line_chars) and line_chars[pos] == " ":
                    line_chars[pos] = "-"

    return re.split("\\s{2,}", "".join(line_chars).rstrip())


async def poll_pod_metrics(resource):
    """Poll pod metrics periodically - converted from recursion to loop."""
    if is_all_namespaces():
        return

    namespace = sys.argv[2]

    while True:
        try:
            # Run blocking command in executor to avoid blocking event loop
            loop = asyncio.get_event_loop()
            output = await loop.run_in_executor(
                None,
                lambda: kube_capacity[
                    f"--namespace {namespace} --util --pods --output json".split(" ")
                ](),
            )
            metrics = json.loads(output)

            for node in metrics["nodes"]:
                if "pods" in node:
                    for pod in node["pods"]:
                        pod["cpu"]["utilization"] = pod["cpu"].get("utilization", "0m")
                        pod["memory"]["utilization"] = pod["memory"].get(
                            "utilization", "0Mi"
                        )
                        cpu_request = int(pod["cpu"]["requests"].rstrip("m"))
                        cpu_util = int(pod["cpu"]["utilization"].rstrip("m"))

                        memory_request = int(pod["memory"]["requests"].rstrip("Mi"))
                        memory_util = int(pod["memory"]["utilization"].rstrip("Mi"))

                        pod_metrics_cache[f"pod/{pod['name']}"] = {
                            "cpu": {
                                "requests": pod["cpu"]["requests"],
                                "limits": pod["cpu"]["limits"],
                                "utilization": f"{percent_as_lines(f'{math.floor(cpu_util / cpu_request * 100)}%')} {pod['cpu']['utilization']}"
                                if cpu_request > 0 and cpu_util > 0
                                else "",
                            },
                            "memory": {
                                "requests": pod["memory"]["requests"],
                                "limits": pod["memory"]["limits"],
                                "utilization": f"{percent_as_lines(f'({math.floor(memory_util / memory_request * 100)}%)')} {pod['memory']['utilization']}"
                                if memory_request > 0 and memory_util > 0
                                else "",
                            },
                        }

            update_pod_output(resource)
        except Exception as e:
            print(f"Error polling pod metrics: {e}")

        await asyncio.sleep(10)


node_metrics_cache = {}
node_podcount_cache = {}
node_gpucount_cache = {}
node_status_cache = {}
node_table = PrettyTable()
node_table.set_style(PLAIN_COLUMNS)
node_table.align = "l"
node_table.right_padding_width = 2


async def poll_podcount():
    """Poll pod count periodically - converted from recursion to loop."""
    while True:
        try:
            # Run blocking command in executor
            loop = asyncio.get_event_loop()
            podcount = await loop.run_in_executor(
                None,
                lambda: (
                    kubectl[
                        "get pods -A --field-selector=status.phase==Running -ojson".split(
                            " "
                        )
                    ]
                    | jq[
                        "-r",
                        """
.items[] | [
  .spec.nodeName,
  .metadata.name,
  ([(
    .spec.containers[]?.resources.limits."nvidia.com/gpu",
    .spec.containers[]?.resources.limits."amd.com/gpu"
  )] | map(tonumber? // 0) | add)
] | @tsv
            """,
                    ]
                )(),  # Call the pipeline to execute it!
            )

            node_cache = {}
            for pc in podcount.split("\n"):
                if pc.strip() != "":
                    [node, pod, gpus] = re.split(r"\t+", pc.strip())

                    if not node_cache.get(node):
                        node_cache[node] = {}

                    if node_cache[node].get("pods"):
                        node_cache[node]["pods"] += [pod]
                    else:
                        node_cache[node]["pods"] = [pod]

                    if node_cache[node].get("gpus"):
                        node_cache[node]["gpus"] += int(gpus)
                    else:
                        node_cache[node]["gpus"] = int(gpus)

            for node_name in node_cache.keys():
                node_podcount_cache[f"node/{node_name}"] = len(
                    node_cache[node_name].get("pods", [])
                )
                node_gpucount_cache[f"node/{node_name}"] = node_cache[node_name].get(
                    "gpus", []
                )

            update_node_output()
        except Exception as e:
            print(f"Error polling podcount: {e}")

        await asyncio.sleep(15)


async def refresh_nodes():
    """Refresh nodes periodically - converted from recursion to loop."""
    while True:
        update_node_output()
        await asyncio.sleep(15)


async def poll_node_metrics():
    """Poll node metrics periodically - converted from recursion to loop."""
    while True:
        try:
            # Run blocking command in executor
            loop = asyncio.get_event_loop()
            output = await loop.run_in_executor(None, lambda: kube_capacity["--util"]())
            node_metrics = output.split("\n")[1:]
            for node_line in node_metrics:
                # skip the limit ones, we don't care
                if node_line.strip() != "":
                    [name, creq, _, cuse, mreq, _, muse] = re.split(
                        "\\s{3,}", node_line.strip()
                    )
                    metrics = [make_metric_pretty(m) for m in [creq, cuse, mreq, muse]]
                    node_metrics_cache[f"node/{name}"] = metrics

            update_node_output()
        except Exception as e:
            print(f"Error polling node metrics: {e}")

        await asyncio.sleep(60)


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
    return min(6, math.ceil(util / 20)) * "|"


async def watch_nodes():
    """Watch nodes asynchronously."""
    p = await asyncio.create_subprocess_exec(
        "kubectl",
        "get",
        "nodes",
        "--watch",
        "--show-kind=true",
        "-o",
        "wide",
        "--label-columns=topology.kubernetes.io/zone,,karpenter.sh/nodepool,karpenter.sh/capacity-type,node.kubernetes.io/instance-type",
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE,
    )

    headers_bytes = await p.stdout.readline()
    headers_line = headers_bytes.decode("utf-8").strip()
    header_start_positions = list(re.finditer("\\s{3}[A-Z]", headers_line))
    headers = re.split("\\s{3,}", headers_line)
    node_table.field_names = [
        "NAME",
        "Pods",
        "CReq",
        "CUse",
        "MReq",
        "MUse",
        "GPUs",
    ] + headers[1:]
    # sort_column = get_sort_column(node_table)
    # node_table.sortby = sort_column
    # if sort_column in SORT_FUNCTIONS:
    #     node_table.sort_key = SORT_FUNCTIONS[sort_column]

    while True:
        [name, *status] = await add_placeholders_async(header_start_positions, p)

        namespace = sys.argv[2]
        if namespace == "buildbarn":
            if "buildbarn" in " ".join(status):
                node_status_cache[name] = status
        else:
            node_status_cache[name] = status

        if p.returncode is not None:
            break


def update_node_output():
    """Update node output - no lock needed with asyncio."""
    node_table.clear_rows()

    total = len(node_status_cache.keys())
    for node_name in node_status_cache.keys():
        if "fargate" in node_name:
            continue

        pods = ""
        if node_name in node_podcount_cache:
            pods = node_podcount_cache[node_name]

        gpus = "."
        if node_name in node_gpucount_cache:
            gpus = node_gpucount_cache[node_name]

        metrics = ["" for _ in range(4)]
        if node_name in node_metrics_cache:
            for index, metric in enumerate(node_metrics_cache[node_name]):
                metrics[index] = metric

        status = node_status_cache[node_name]

        row = [node_name, pods] + metrics + [gpus] + status
        header_length = len(node_table.field_names)
        while header_length > len(row):
            row = row + [""]

        if IS_GREP and GREP_NEEDLE != "karpenter":
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

# Run the async main function
asyncio.run(main())
