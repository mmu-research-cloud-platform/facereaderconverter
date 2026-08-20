#include <Rcpp.h>
#include <RcppParallel.h>

#include <algorithm>
#include <string>
#include <unordered_map>
#include <vector>

using namespace Rcpp;
using namespace RcppParallel;

namespace {

struct DenominatorFrame {
  std::string emotion;
  std::string video_time;
  int run_id;
};

struct FrameStatus {
  bool present;
  bool in_state;
};

struct EpisodeSummary {
  std::string id;
  std::string denominator;
  std::string numerator;
  std::string emotion;
  int n_episodes;
  double synchrony;
};

struct GroupInput {
  std::string id;
  std::vector<std::string> subjects;
  std::unordered_map<std::string, std::vector<std::string> > denominator_emotions;
  std::unordered_map<std::string, std::vector<DenominatorFrame> > denominator_frames;
  std::unordered_map<
    std::string,
    std::unordered_map<std::string, FrameStatus>
  > comparison_lookup;
};

void push_unique(std::vector<std::string>& x, const std::string& value) {
  if (std::find(x.begin(), x.end(), value) == x.end()) {
    x.push_back(value);
  }
}

std::string make_frame_key(
    const std::string& emotion,
    const std::string& video_time) {
  return emotion + "\r" + video_time;
}

std::string make_episode_key(const std::string& emotion, int run_id) {
  return emotion + "\r" + std::to_string(run_id);
}

struct SynchronyWorker : public Worker {
  const std::vector<GroupInput>& groups;
  const double missing_threshold;
  std::vector<std::vector<EpisodeSummary> >& results_by_group;
  std::vector<std::string>& singleton_by_group;

  SynchronyWorker(
      const std::vector<GroupInput>& groups,
      double missing_threshold,
      std::vector<std::vector<EpisodeSummary> >& results_by_group,
      std::vector<std::string>& singleton_by_group)
    : groups(groups),
      missing_threshold(missing_threshold),
      results_by_group(results_by_group),
      singleton_by_group(singleton_by_group) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t idx = begin; idx < end; ++idx) {
      const GroupInput& group = groups[idx];
      std::vector<EpisodeSummary>& group_results = results_by_group[idx];
      const std::vector<std::string>& subjects = group.subjects;

      if (subjects.size() < 2) {
        if (subjects.size() == 1) {
          singleton_by_group[idx] =
            std::string("id ") + group.id + ": " + subjects[0];
        }
        continue;
      }

      for (std::size_t d = 0; d < subjects.size(); ++d) {
        const std::string& denominator_subject = subjects[d];

        std::unordered_map<
          std::string,
          std::vector<std::string>
        >::const_iterator emotion_it =
          group.denominator_emotions.find(denominator_subject);
        if (
          emotion_it == group.denominator_emotions.end() ||
            emotion_it->second.empty()
        ) {
          continue;
        }

        std::unordered_map<
          std::string,
          std::vector<DenominatorFrame>
        >::const_iterator frame_it =
          group.denominator_frames.find(denominator_subject);
        if (
          frame_it == group.denominator_frames.end() ||
            frame_it->second.empty()
        ) {
          continue;
        }

        const std::vector<std::string>& base_emotions = emotion_it->second;
        const std::vector<DenominatorFrame>& denominator_frames = frame_it->second;

        for (std::size_t n = 0; n < subjects.size(); ++n) {
          if (n == d) {
            continue;
          }

          const std::string& numerator_subject = subjects[n];
          std::unordered_map<std::string, int> total_count;
          std::unordered_map<std::string, int> present_count;
          std::unordered_map<std::string, bool> synchrony_flag;
          std::unordered_map<std::string, std::string> episode_emotion;

          std::unordered_map<
            std::string,
            std::unordered_map<std::string, FrameStatus>
          >::const_iterator numerator_it =
            group.comparison_lookup.find(numerator_subject);

          for (std::size_t i = 0; i < denominator_frames.size(); ++i) {
            const DenominatorFrame& frame = denominator_frames[i];
            const std::string episode_key = make_episode_key(
              frame.emotion,
              frame.run_id
            );
            episode_emotion[episode_key] = frame.emotion;
            total_count[episode_key] += 1;

            bool present = false;
            bool in_state = false;

            if (numerator_it != group.comparison_lookup.end()) {
              const std::string frame_key = make_frame_key(
                frame.emotion,
                frame.video_time
              );
              std::unordered_map<std::string, FrameStatus>::const_iterator status_it =
                numerator_it->second.find(frame_key);
              if (status_it != numerator_it->second.end()) {
                present = status_it->second.present;
                in_state = status_it->second.in_state;
              }
            }

            if (present) {
              present_count[episode_key] += 1;
            }
            if (in_state) {
              synchrony_flag[episode_key] = true;
            }
          }

          std::unordered_map<std::string, int> episode_count_by_emotion;
          std::unordered_map<std::string, int> numerator_count_by_emotion;

          for (
            std::unordered_map<std::string, int>::const_iterator total_it =
              total_count.begin();
            total_it != total_count.end();
            ++total_it
          ) {
            if (total_it->second == 0) {
              continue;
            }

            int present = 0;
            std::unordered_map<std::string, int>::const_iterator present_it =
              present_count.find(total_it->first);
            if (present_it != present_count.end()) {
              present = present_it->second;
            }

            const double present_prop =
              static_cast<double>(present) / static_cast<double>(total_it->second);
            if (present_prop < missing_threshold) {
              continue;
            }

            const std::string& emotion = episode_emotion[total_it->first];
            episode_count_by_emotion[emotion] += 1;

            std::unordered_map<std::string, bool>::const_iterator flag_it =
              synchrony_flag.find(total_it->first);
            if (flag_it != synchrony_flag.end() && flag_it->second) {
              numerator_count_by_emotion[emotion] += 1;
            }
          }

          for (std::size_t i = 0; i < base_emotions.size(); ++i) {
            const std::string& emotion = base_emotions[i];

            int n_episodes = 0;
            std::unordered_map<std::string, int>::const_iterator count_it =
              episode_count_by_emotion.find(emotion);
            if (count_it != episode_count_by_emotion.end()) {
              n_episodes = count_it->second;
            }

            int numerator_count = 0;
            std::unordered_map<std::string, int>::const_iterator numerator_count_it =
              numerator_count_by_emotion.find(emotion);
            if (numerator_count_it != numerator_count_by_emotion.end()) {
              numerator_count = numerator_count_it->second;
            }

            EpisodeSummary out;
            out.id = group.id;
            out.denominator = denominator_subject;
            out.numerator = numerator_subject;
            out.emotion = emotion;
            out.n_episodes = n_episodes;
            out.synchrony = n_episodes > 0 ?
              static_cast<double>(numerator_count) / static_cast<double>(n_episodes) :
              NA_REAL;
            group_results.push_back(out);
          }
        }
      }
    }
  }
};

} // namespace

// [[Rcpp::export]]
List synchrony_cpp(DataFrame coding, DataFrame episodes, double missing_threshold) {
  CharacterVector coding_id = as<CharacterVector>(coding["id"]);
  CharacterVector coding_subject = as<CharacterVector>(coding["subject"]);
  CharacterVector coding_emotion = as<CharacterVector>(coding["emotion"]);
  CharacterVector coding_video_time = as<CharacterVector>(coding["video_time"]);
  LogicalVector coding_in_state = coding["in_state"];
  NumericVector coding_value = coding["value"];
  IntegerVector coding_run_id = coding["run_id"];

  CharacterVector episodes_id = as<CharacterVector>(episodes["id"]);
  CharacterVector episodes_subject = as<CharacterVector>(episodes["subject"]);
  CharacterVector episodes_emotion = as<CharacterVector>(episodes["emotion"]);

  std::unordered_map<std::string, std::size_t> group_index;
  std::vector<GroupInput> groups;

  for (int i = 0; i < coding.nrows(); ++i) {
    const std::string id = as<std::string>(coding_id[i]);
    const std::string subject = as<std::string>(coding_subject[i]);
    const std::string emotion = as<std::string>(coding_emotion[i]);

    std::unordered_map<std::string, std::size_t>::const_iterator idx_it =
      group_index.find(id);
    if (idx_it == group_index.end()) {
      GroupInput group;
      group.id = id;
      groups.push_back(group);
      group_index[id] = groups.size() - 1;
      idx_it = group_index.find(id);
    }

    GroupInput& group = groups[idx_it->second];
    push_unique(group.subjects, subject);

    FrameStatus status;
    status.present = !NumericVector::is_na(coding_value[i]);
    status.in_state =
      !LogicalVector::is_na(coding_in_state[i]) &&
      static_cast<bool>(coding_in_state[i]);

    const std::string video_time = as<std::string>(coding_video_time[i]);

    group.comparison_lookup[subject][make_frame_key(emotion, video_time)] =
      status;

    if (status.in_state) {
      DenominatorFrame frame;
      frame.emotion = emotion;
      frame.video_time = video_time;
      frame.run_id = coding_run_id[i];
      group.denominator_frames[subject].push_back(frame);
    }
  }

  for (int i = 0; i < episodes.nrows(); ++i) {
    const std::string id = as<std::string>(episodes_id[i]);
    std::unordered_map<std::string, std::size_t>::const_iterator idx_it =
      group_index.find(id);
    if (idx_it == group_index.end()) {
      continue;
    }

    GroupInput& group = groups[idx_it->second];
    const std::string subject = as<std::string>(episodes_subject[i]);
    const std::string emotion = as<std::string>(episodes_emotion[i]);
    push_unique(group.denominator_emotions[subject], emotion);
  }

  std::vector<std::vector<EpisodeSummary> > results_by_group(groups.size());
  std::vector<std::string> singleton_by_group(groups.size());

  SynchronyWorker worker(
    groups,
    missing_threshold,
    results_by_group,
    singleton_by_group
  );
  parallelFor(static_cast<std::size_t>(0), groups.size(), worker);

  std::vector<EpisodeSummary> results;
  std::size_t total_size = 0;
  for (std::size_t i = 0; i < results_by_group.size(); ++i) {
    total_size += results_by_group[i].size();
  }
  results.reserve(total_size);

  for (std::size_t i = 0; i < results_by_group.size(); ++i) {
    results.insert(
      results.end(),
      results_by_group[i].begin(),
      results_by_group[i].end()
    );
  }

  CharacterVector out_id(results.size());
  CharacterVector out_denominator(results.size());
  CharacterVector out_numerator(results.size());
  CharacterVector out_emotion(results.size());
  IntegerVector out_n_episodes(results.size());
  NumericVector out_synchrony(results.size());

  for (std::size_t i = 0; i < results.size(); ++i) {
    out_id[i] = results[i].id;
    out_denominator[i] = results[i].denominator;
    out_numerator[i] = results[i].numerator;
    out_emotion[i] = results[i].emotion;
    out_n_episodes[i] = results[i].n_episodes;
    out_synchrony[i] = results[i].synchrony;
  }

  DataFrame out = DataFrame::create(
    _["id"] = out_id,
    _["denominator"] = out_denominator,
    _["numerator"] = out_numerator,
    _["emotion"] = out_emotion,
    _["n_episodes"] = out_n_episodes,
    _["synchrony"] = out_synchrony
  );
  out.attr("stringsAsFactors") = false;

  std::vector<std::string> singleton_ids_vec;
  singleton_ids_vec.reserve(singleton_by_group.size());
  for (std::size_t i = 0; i < singleton_by_group.size(); ++i) {
    if (!singleton_by_group[i].empty()) {
      singleton_ids_vec.push_back(singleton_by_group[i]);
    }
  }

  return List::create(
    _["result"] = out,
    _["singleton_ids"] = wrap(singleton_ids_vec)
  );
}
