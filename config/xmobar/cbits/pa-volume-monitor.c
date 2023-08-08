#include <assert.h>
#include <pulse/pulseaudio.h>
#include <stdbool.h>
#include <stdio.h>

static void context_state_callback(pa_context *c, void *userdata);
static void server_info_callback(pa_context *c, const pa_server_info *i, void *userdata);
static void subscribe_callback(pa_context *c, pa_subscription_event_type_t type, uint32_t idx, void *userdata);
static void sink_info_callback(pa_context *c, const pa_sink_info *i, int eol, void *userdata);

typedef void (VolumeCallback)(float,bool);

struct PaVolumeMonitor {
	pa_mainloop* mainloop;
	pa_mainloop_api* mainloop_api;
	pa_context* context;
	VolumeCallback *volume_callback;
};

struct PaVolumeMonitor* pa_volume_monitor_init()
{
	struct PaVolumeMonitor *volume_monitor = malloc(sizeof(struct PaVolumeMonitor));

	volume_monitor->mainloop = pa_mainloop_new();
	if (!volume_monitor->mainloop) {
		return NULL;
	}

	volume_monitor->mainloop_api = pa_mainloop_get_api(volume_monitor->mainloop);

	volume_monitor->context = pa_context_new(volume_monitor->mainloop_api, "PulseAudio Test");
	if (!volume_monitor->context) {
		return NULL;
	}

	return volume_monitor;
}

void pa_volume_monitor_set_callback(struct PaVolumeMonitor *volume_monitor, VolumeCallback *volume_callback)
{
	volume_monitor->volume_callback = volume_callback;
}

int pa_volume_monitor_run(struct PaVolumeMonitor *volume_monitor)
{
	if (pa_context_connect(volume_monitor->context, NULL, PA_CONTEXT_NOAUTOSPAWN, NULL) < 0) {
		return EXIT_FAILURE;
	}

	pa_context_set_state_callback(volume_monitor->context, context_state_callback, volume_monitor);

	int return_value;

	if (pa_mainloop_run(volume_monitor->mainloop, &return_value) < 0) {
		return return_value;
	}

	return return_value;
}

const char* pa_volume_monitor_get_error(struct PaVolumeMonitor *volume_monitor)
{
	int error = pa_context_errno(volume_monitor->context);
	return pa_strerror(error);
}

void pa_volume_monitor_free(struct PaVolumeMonitor *volume_monitor)
{
	if (volume_monitor->context) {
		pa_context_unref(volume_monitor->context);
		volume_monitor->context = NULL;
	}

	if (volume_monitor->mainloop) {
		pa_mainloop_free(volume_monitor->mainloop);
		volume_monitor->mainloop = NULL;
		volume_monitor->mainloop_api = NULL;
	}

	free(volume_monitor);
}

static void context_state_callback(pa_context *c, void *userdata)
{
	assert(userdata);
	struct PaVolumeMonitor *volume_monitor = (struct PaVolumeMonitor*) userdata;

	switch (pa_context_get_state(c))
	{
		case PA_CONTEXT_CONNECTING:
		case PA_CONTEXT_AUTHORIZING:
		case PA_CONTEXT_SETTING_NAME:
			break;

		case PA_CONTEXT_READY:
			pa_context_get_server_info(c, server_info_callback, userdata);
			pa_context_set_subscribe_callback(c, subscribe_callback, userdata);
			pa_context_subscribe(c, PA_SUBSCRIPTION_MASK_SINK, NULL, NULL);
			break;

		case PA_CONTEXT_TERMINATED:
			volume_monitor->mainloop_api->quit(volume_monitor->mainloop_api, EXIT_SUCCESS);
			break;

		case PA_CONTEXT_FAILED:
		default:
			volume_monitor->mainloop_api->quit(volume_monitor->mainloop_api, EXIT_FAILURE);
			break;
	}
}

static void server_info_callback(
	pa_context *c,
	const pa_server_info *i,
	void *userdata
) {
	pa_context_get_sink_info_by_name(c, i->default_sink_name, sink_info_callback, userdata);
}

static void subscribe_callback(
	pa_context *c,
	pa_subscription_event_type_t type,
	uint32_t idx,
	void *userdata
) {
	unsigned facility = type & PA_SUBSCRIPTION_EVENT_FACILITY_MASK;

	switch (facility)
	{
		case PA_SUBSCRIPTION_EVENT_SINK:
			pa_operation *op = pa_context_get_sink_info_by_index(c, idx, sink_info_callback, userdata);
			if (op) {
				pa_operation_unref(op);
			}
			break;

		default:
			assert(0);  // Got unexpected event.
			break;
	}
}

static void sink_info_callback(
	pa_context *,
	const pa_sink_info *i,
	int eol,
	void *userdata
) {
	if (!i) {
		return;
	}

	assert(userdata);
	struct PaVolumeMonitor *volume_monitor = (struct PaVolumeMonitor*) userdata;

	if (volume_monitor->volume_callback) {
		float volume = (float) pa_cvolume_avg(&i->volume) / PA_VOLUME_NORM;
		volume_monitor->volume_callback(volume, i->mute);
	}
}
